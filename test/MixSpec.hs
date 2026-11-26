{-# LANGUAGE OverloadedStrings #-}
module MixSpec (mixTests) where

import Control.Exception (SomeException, catch, displayException)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Storable as V

import Sonora.DSL.Evaluator (evaluateSong)
import Sonora.DSL.Parser (parseSonora)
import Sonora.Project.Compile
import Sonora.Project.Mix
import Sonora.Project.Render
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Render.Engine (Song (..), normalizeSignal, renderTrack)
import Sonora.Signal.Core
import Sonora.Synth.Patch (lookupPresetPatch)

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

stem :: [(Double, Double)] -> V.Vector Frame
stem = V.fromList . map (uncurry Frame)

a, b :: V.Vector Frame
a = stem [(0.2, 0.1), (-0.3, 0.4), (0.1, 0.1)]
b = stem [(0.05, -0.05), (0.1, 0.2)]  -- shorter

mix :: TrackMix
mix = defaultTrackMix

near :: V.Vector Frame -> [(Double, Double)] -> Bool
near v xs = V.length v == length xs && and [ abs (l - x) < 1e-12 && abs (r - y) < 1e-12 | (Frame l r, (x, y)) <- zip (V.toList v) xs ]

score :: Text
score = T.unlines
    [ "tempo 120"
    , "track lead (lead):", "    C4 quarter", "    E4 quarter"
    , "track bass (bass):", "    C2 half"
    , "track pad (pad):", "    chord(C4, E4, G4) half" ]

trackIdOf :: Text -> Project -> TrackId
trackIdOf name p = case [ trackId t | t <- projectTracks p, trackScoreName t == name ] of
    (i : _) -> i
    [] -> error ("no track " ++ T.unpack name)

-- | A project after commands, which must all succeed.
after :: [Project -> Command] -> Project -> Project
after cs p0 = foldl (\p c -> either (error . show) id (applyCommand (c p) p)) p0 cs

-- | Render with a synthesizer that counts the stems it makes.
counting :: IORef Int -> Int -> CompiledProject -> CompiledTrack -> IO (V.Vector Frame)
counting n sr c t = modifyIORef' n (+ 1) >> renderStem sr c t

mixTests :: IO Bool
mixTests = do
    putStrLn "\n--- Mixing and stems ---"
    let sr = 4000
        p0 = newProject score
        compiled = fromJust . compileProject
    rs <- sequence
        [ check "the default mix is the plain sum, padded to the longest track" $
            return (near (mixdown 1 (MasterMix 1) [(mix, a), (mix, b)]) [(0.25, 0.05), (-0.2, 0.6), (0.1, 0.1)])
        , check "gain and master scale exactly" $
            return (near (mixdown 1 (MasterMix 0.5) [(mix { mixGain = 0.5 }, a), (mix, b)])
                         [ (0.5 * (0.5 * x + y), 0.5 * (0.5 * x' + y')) | ((x, x'), (y, y')) <- zip [(0.2, 0.1), (-0.3, 0.4), (0.1, 0.1)] [(0.05, -0.05), (0.1, 0.2), (0, 0)] ])
        , check "pan is equal power: hard right is silent on the left, centre is unity" $ do
            let (l, r) = panGains 1 1
                (cl, cr) = panGains 0.8 0
                (ll, lr) = panGains 1 (-0.5)
            return (abs l < 1e-12 && abs (r - sqrt 2) < 1e-12 && cl == 0.8 && cr == 0.8 && abs (ll * ll + lr * lr - 2) < 1e-12 && ll > lr)
        , check "mute and solo: solo wins, mute always silences" $
            return (and
                [ audible [mix, mix] == [True, True]
                , audible [mix { mixMute = True }, mix] == [False, True]
                , audible [mix { mixSolo = True }, mix] == [True, False]
                , audible [mix { mixSolo = True, mixMute = True }, mix] == [False, False]
                , audible [mix { mixSolo = True }, mix { mixSolo = True }, mix] == [True, True, False]
                , near (mixdown 1 (MasterMix 1) [(mix { mixSolo = True }, a), (mix, b)]) [(0.2, 0.1), (-0.3, 0.4), (0.1, 0.1)]
                , near (mixdown 1 (MasterMix 1) [(mix { mixMute = True }, a), (mix { mixMute = True }, b)]) [(0, 0), (0, 0), (0, 0)] ])
        , check "the limiter leaves 0.95 alone and never goes past full scale" $ do
            let loud = mixdown 1 (MasterMix 1) [(mix { mixGain = 40 }, a)]
                xs = concat [ [l, r] | Frame l r <- V.toList loud ]
            return (limitSample 0.95 == 0.95 && limitSample (-0.5) == -0.5 && all ((<= 1) . abs) xs
                    && limitSample 1 < limitSample 1.05 && limitSample 1.05 < 1 && limitSample (-1.05) == negate (limitSample 1.05))
        , check "the song gain depends on the stems only, so a solo is heard at its level" $ do
            let g = songGain [a, b]
                full = mixdown g (MasterMix 1) [(mix, a), (mix, b)]
                solo = mixdown g (MasterMix 1) [(mix { mixSolo = True }, a), (mix, b)]
                peak v = V.maximum (V.map (\(Frame l r) -> max (abs l) (abs r)) v)
            return (abs (peak full - 0.95) < 1e-12 && peak solo < peak full && abs (peak solo - g * 0.4) < 1e-12)
        , check "at the default mix the app renders the tracks' sum normalised to 0.95, bit for bit" $ do
            txt <- TIO.readFile "examples/synthwave.sonora"
            let c = compiled (newProject txt)
                cfg = defaultConfig { configSampleRate = sr }
            out <- renderCompiled sr (\_ _ -> return ()) c
            return $ case parseSonora "x" (T.unpack txt) of
                Left _ -> False
                Right ast ->
                    let song = evaluateSong ast
                        tracks = map (renderTrack (songBPM song)) (songTracks song)
                    in out == runSignal (normalizeSignal 0.95 (foldl mixSignals emptySignal tracks)) cfg
        , check "a mix change reuses every stem, and remixing equals rendering afresh" $ do
            n <- newIORef 0
            (cache, _) <- renderProjectWith (counting n) sr (\_ _ -> return ()) emptyStems (compiled p0)
            first <- readIORef n
            let p1 = after [\p -> UpdateTrackMix (trackIdOf "bass" p) noMixUpdate { updateGain = Just 0.3, updatePan = Just (-0.6) }, const (SetMasterGain 0.7)] p0
            (_, remixed) <- renderProjectWith (counting n) sr (\_ _ -> return ()) cache (compiled p1)
            again <- readIORef n
            fresh <- renderCompiled sr (\_ _ -> return ()) (compiled p1)
            return (first == 3 && again == 3 && remixed == fresh)
        , check "a sound change renders only that track" $ do
            n <- newIORef 0
            (cache, _) <- renderProjectWith (counting n) sr (\_ _ -> return ()) emptyStems (compiled p0)
            let p1 = after [\p -> SetTrackPatch (trackIdOf "pad" p) (fromJust (lookupPresetPatch "chime"))] p0
            steps <- newIORef []
            (_, out) <- renderProjectWith (counting n) sr (\d t -> modifyIORef' steps ((d, t) :)) cache (compiled p1)
            fresh <- renderCompiled sr (\_ _ -> return ()) (compiled p1)
            total <- readIORef n
            progress <- reverse <$> readIORef steps
            return (total == 4 && out == fresh && progress == [(0, 2), (1, 2), (2, 2)])
        , check "reordering tracks reuses stems; removing one drops its stem" $ do
            n <- newIORef 0
            (cache, _) <- renderProjectWith (counting n) sr (\_ _ -> return ()) emptyStems (compiled p0)
            let reordered = T.unlines
                    [ "tempo 120"
                    , "track pad (pad):", "    chord(C4, E4, G4) half"
                    , "track lead (lead):", "    C4 quarter", "    E4 quarter"
                    , "track bass (bass):", "    C2 half" ]
                removed = T.unlines [ "tempo 120", "track lead (lead):", "    C4 quarter", "    E4 quarter" ]
                p1 = after [const (SetScoreText reordered)] p0
                p2 = after [const (SetScoreText removed)] p1
            (cache1, out1) <- renderProjectWith (counting n) sr (\_ _ -> return ()) cache (compiled p1)
            afterReorder <- readIORef n
            (cache2, _) <- renderProjectWith (counting n) sr (\_ _ -> return ()) cache1 (compiled p2)
            afterRemove <- readIORef n
            fresh1 <- renderCompiled sr (\_ _ -> return ()) (compiled p1)
            return (afterReorder == 3 && afterRemove == 3 && stemCount cache2 == 1 && out1 == fresh1)
        , check "every mix control changes the output (none is decorative)" $ do
            (cache, base) <- renderProject sr (\_ _ -> return ()) emptyStems (compiled p0)
            let variants =
                    [ \p -> UpdateTrackMix (trackIdOf "lead" p) noMixUpdate { updateGain = Just 0.5 }
                    , \p -> UpdateTrackMix (trackIdOf "lead" p) noMixUpdate { updatePan = Just 0.5 }
                    , \p -> UpdateTrackMix (trackIdOf "lead" p) noMixUpdate { updateMute = Just True }
                    , \p -> UpdateTrackMix (trackIdOf "lead" p) noMixUpdate { updateSolo = Just True }
                    , const (SetMasterGain 0.5) ]
            outs <- mapM (\c -> snd <$> renderProject sr (\_ _ -> return ()) cache (compiled (after [c] p0))) variants
            return (all (/= base) outs)
        ]
    return (and rs)
