{-# LANGUAGE OverloadedStrings #-}
module SessionSpec (sessionTests) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM, when)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Storable as V
import System.Timeout (timeout)

import Sonora.Audio.Backend
import Sonora.Audio.Fake
import Sonora.DSL.Evaluator (evaluateSong)
import Sonora.DSL.Parser (parseSonora)
import Sonora.Project.Compile
import Sonora.Project.Render (renderCompiled)
import Sonora.Project.Types (Command (..), MixUpdate (..), Revision (..), TrackId (..), noMixUpdate)
import Sonora.Project.Update (newProject)
import Sonora.Render.Engine (normalizeSignal, renderSong)
import Sonora.Service.Methods (applicationMethods, serialMethods)
import Sonora.Service.Protocol
import Sonora.Service.Session
import Sonora.Signal.Core

-------------------------------------------------------------------------------
-- Harness

data Harness = Harness
    { hSession :: Session
    , hEmit    :: Emit
    , hEvents  :: IORef [Event]          -- newest first
    , hFake    :: FakeAudio
    , hOpens   :: IORef Int
    }

data Fakes = Fakes
    { renderDelayMs :: Int
    , renderFails   :: Bool
    , audioFails    :: Bool
    }

fakes :: Fakes
fakes = Fakes 0 False False

-- | Frames whose value identifies the revision they were rendered from.
fakeRenderer :: Fakes -> Int -> (Int -> Int -> IO ()) -> CompiledProject -> IO (V.Vector Frame)
fakeRenderer f _ progress c = do
    progress 0 1
    when (renderDelayMs f > 0) $ threadDelay (renderDelayMs f * 1000)
    when (renderFails f) $ throwIO (userError "synthesis exploded")
    progress 1 1
    let Revision r = compiledRevision c
        x = fromIntegral r / 100
    return (V.replicate 4000 (Frame x x))

withHarness :: Fakes -> (Harness -> IO Bool) -> IO Bool
withHarness f act = do
    eventLog <- newIORef []
    opens <- newIORef (0 :: Int)
    fake <- newFakeAudio 1000
    let cfg = defaultSessionConfig
            { sessionSampleRate      = 1000
            , sessionRenderer        = fakeRenderer f
            , sessionOpenAudio       = \_ -> do
                modifyIORef' opens (+ 1)
                return $ if audioFails f then Left (AudioError 1 "no output device") else Right (fakeBackend fake)
            , sessionInitialScore    = scoreA
            , sessionTelemetryMicros = 20000
            }
    s <- newSession cfg
    let emit e = atomicModifyIORef' eventLog (\es -> (e : es, ()))
    act (Harness s emit eventLog fake opens) `finally` closeSession s

scoreA, scoreB, scoreC, invalidScore :: Text
scoreA = "track lead (lead):\n    C4 quarter\n"
scoreB = "track lead (lead):\n    D4 quarter\n"
scoreC = "track lead (lead):\n    E4 quarter\n"
invalidScore = "track lead (lead\n    C4 quarter\n"

path :: [Text] -> Value -> Maybe Value
path [] v = Just v
path (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= path ks
path _ _ = Nothing

str :: [Text] -> Value -> Maybe Text
str ks v = case path ks v of
    Just (String t) -> Just t
    _ -> Nothing

num :: [Text] -> Value -> Maybe Double
num ks v = case path ks v of
    Just (Number n) -> Just (realToFrac n)
    _ -> Nothing

events :: Harness -> IO [Event]
events h = reverse <$> readIORef (hEvents h)

named :: Text -> [Event] -> [Value]
named n = map eventData . filter ((== n) . eventName)

jobIds :: Text -> [Event] -> [Double]
jobIds n = mapMaybe (num ["jobId"]) . named n

-- | Wait until the event log satisfies a condition (at most 3 s).
waitUntil :: Harness -> ([Event] -> Bool) -> IO Bool
waitUntil h cond = fmap (maybe False (const True)) $ timeout 3000000 $
    let loop = do
            es <- events h
            if cond es then return () else threadDelay 5000 >> loop
    in loop

finishedCount :: Int -> [Event] -> Bool
finishedCount n es = length (named "render.finished" es) >= n

code :: Either ServiceError a -> ErrorCode
code = either errorCode (const InternalError)

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

-------------------------------------------------------------------------------
-- Scenarios

scenarios :: [(String, IO Bool)]
scenarios =
    [ ("Renderer matches the CLI render bit for bit and reports progress", do
        txt <- TIO.readFile "examples/synthwave.sonora"
        steps <- newIORef []
        let compiled = fromJust (compileProject (newProject txt))
        out <- renderCompiled 8000 (\d t -> modifyIORef' steps ((d, t) :)) compiled
        progress <- reverse <$> readIORef steps
        return $ case parseSonora "x" (T.unpack txt) of
            Left _ -> False
            Right ast ->
                out == runSignal (normalizeSignal 0.95 (renderSong (evaluateSong ast))) (AudioConfig 8000 2)
                && progress == [(0, 4), (1, 4), (2, 4), (3, 4), (4, 4)])

    , ("Render is tagged with job and revision and is current", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        done <- waitUntil h (finishedCount 1)
        view <- projectView (hSession h)
        es <- events h
        return $ done && jobIds "render.started" es == [1]
            && case named "render.finished" es of
                [fin] -> num ["jobId"] fin == Just 1 && num ["revision"] fin == Just 0
                         && path ["current"] fin == Just (Bool True)
                _ -> False
            && str ["render", "state"] view == Just "ready"
            && not (null (named "render.progress" es)))

    , ("Invalid edits keep the render current and start no work", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        view <- setScore (hSession h) (hEmit h) invalidScore
        threadDelay 50000
        es <- events h
        return $ length (named "render.started" es) == 1
            && path ["scoreIsCurrent"] view == Just (Bool False)
            && str ["render", "state"] view == Just "ready"
            && case path ["diagnostics"] view of
                Just (Array a) -> length a == 1
                _ -> False)

    , ("Rapid edits cancel superseded renders; only the last finishes", withHarness fakes { renderDelayMs = 200 } $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- setScore (hSession h) (hEmit h) scoreB
        _ <- setScore (hSession h) (hEmit h) scoreC
        done <- waitUntil h (finishedCount 1)
        threadDelay 300000
        es <- events h
        view <- projectView (hSession h)
        return $ done
            && jobIds "render.started" es == [1, 2, 3]
            && jobIds "render.cancelled" es == [1, 2]
            && jobIds "render.finished" es == [3]
            && num ["render", "latest", "revision"] view == num ["revision"] view
            && str ["render", "state"] view == Just "ready")

    , ("Returning to already-rendered content cancels the pending render", withHarness fakes { renderDelayMs = 100 } $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- setScore (hSession h) (hEmit h) scoreB
        view <- setScore (hSession h) (hEmit h) scoreA
        threadDelay 250000
        es <- events h
        return $ jobIds "render.started" es == [1, 2] && jobIds "render.cancelled" es == [2]
            && jobIds "render.finished" es == [1] && str ["render", "state"] view == Just "ready")

    , ("Render failures are reported, retried on request, and block playback", withHarness fakes { renderFails = True } $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        failed <- waitUntil h (\es -> length (named "render.failed" es) == 1)
        view <- projectView (hSession h)
        played <- play (hSession h) (hEmit h)
        _ <- startRender (hSession h) (hEmit h)
        retried <- waitUntil h (\es -> length (named "render.failed" es) == 2)
        es <- events h
        return $ failed && retried
            && str ["render", "state"] view == Just "failed"
            && maybe False (T.isInfixOf "synthesis exploded") (str ["render", "failure", "message"] view)
            && either errorCode (const InternalError) played == NoRender
            && jobIds "render.failed" es == [1, 2])

    , ("Explicit cancel stops the running render", withHarness fakes { renderDelayMs = 500 } $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        r <- cancelRender (hSession h) (hEmit h)
        threadDelay 700000
        es <- events h
        return $ str ["state"] r == Just "idle" && jobIds "render.cancelled" es == [1]
            && null (named "render.finished" es))

    , ("Play, pause, and stop drive the song voice", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        r <- play (hSession h) (hEmit h)
        let b = fakeBackend (hFake h)
        fakeAdvance (hFake h) 500
        playing <- backendStatus b SongVoice
        telemetry <- waitUntil h (\es -> length (named "transport" es) >= 3)
        p <- pause (hSession h) (hEmit h)
        paused <- backendStatus b SongVoice
        st <- stop (hSession h) (hEmit h)
        stopped <- backendStatus b SongVoice
        return $ either (const False) (const True) r && telemetry
            && statusPlaying playing && statusPosition playing == 500
            && path ["playing"] p == Just (Bool False) && statusPosition paused == 500
            && not (statusPlaying stopped) && statusPosition stopped == 0
            && num ["positionSeconds"] st == Just 0 && num ["durationSeconds"] st == Just 4)

    , ("A newer render is swapped in during playback at the same position", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play (hSession h) (hEmit h)
        fakeAdvance (hFake h) 1200
        view <- setScore (hSession h) (hEmit h) scoreB
        _ <- waitUntil h (finishedCount 2)
        frames <- fakeFrames (hFake h) SongVoice
        status <- backendStatus (fakeBackend (hFake h)) SongVoice
        let expected = maybe (-1) (/ 100) (num ["revision"] view)
        return $ V.head frames == Frame expected expected
            && statusPlaying status && statusPosition status == 1200)

    , ("Playing at the end restarts from the beginning", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play (hSession h) (hEmit h)
        fakeAdvance (hFake h) 5000
        ended <- backendStatus (fakeBackend (hFake h)) SongVoice
        _ <- play (hSession h) (hEmit h)
        again <- backendStatus (fakeBackend (hFake h)) SongVoice
        return $ not (statusPlaying ended) && statusPlaying again && statusPosition again == 0)

    , ("Missing audio output is reported and retried on the next play", withHarness fakes { audioFails = True } $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        r1 <- play (hSession h) (hEmit h)
        r2 <- play (hSession h) (hEmit h)
        opens <- readIORef (hOpens h)
        return $ either errorCode (const InternalError) r1 == AudioUnavailable
            && either errorMessage (const "") r1 == "no output device"
            && either errorCode (const InternalError) r2 == AudioUnavailable && opens == 2)

    , ("Seek moves playback, also before the device is open", withHarness fakes $ \h -> do
        early <- seek (hSession h) (hEmit h) 1
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        v1 <- seek (hSession h) (hEmit h) 1.5
        _ <- play (hSession h) (hEmit h)
        started <- backendStatus (fakeBackend (hFake h)) SongVoice
        _ <- seek (hSession h) (hEmit h) 3
        moved <- backendStatus (fakeBackend (hFake h)) SongVoice
        v2 <- seek (hSession h) (hEmit h) 99
        bad <- mapM (seek (hSession h) (hEmit h)) [-1, 0 / 0, 1 / 0]
        return $ code early == NoRender
            && (either (const Nothing) (num ["positionSeconds"]) v1) == Just 1.5
            && statusPosition started == 1500 && statusPlaying started
            && statusPosition moved == 3000 && statusPlaying moved
            && either (const Nothing) (num ["positionSeconds"]) v2 == Just 4
            && all ((== InvalidValue) . code) bad)

    , ("Loops wrap sample-accurately, survive re-rendering, and validate", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- setLoop (hSession h) (hEmit h) (Just (1, 2))
        _ <- seek (hSession h) (hEmit h) 1.9
        _ <- play (hSession h) (hEmit h)
        fakeAdvance (hFake h) 150
        wrapped <- backendStatus (fakeBackend (hFake h)) SongVoice
        _ <- setScore (hSession h) (hEmit h) scoreB
        _ <- waitUntil h (finishedCount 2)
        fakeAdvance (hFake h) 1000
        after <- backendStatus (fakeBackend (hFake h)) SongVoice
        beyond <- setLoop (hSession h) (hEmit h) (Just (10, 12))
        cleared <- setLoop (hSession h) (hEmit h) Nothing
        bad <- mapM (setLoop (hSession h) (hEmit h) . Just) [(2, 1), (1, 1.01), (-1, 2), (0, 1 / 0)]
        return $ statusPosition wrapped == 1050 && statusPosition after == 1050 && statusPlaying after
            && either (const Nothing) (path ["loopActive"]) beyond == Just (Bool False)
            && either (const Nothing) (path ["loop"]) cleared == Just Null
            && all ((== InvalidValue) . code) bad)

    , ("Volume applies to the device and validates", withHarness fakes $ \h -> do
        before <- setVolume (hSession h) (hEmit h) 0.25
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play (hSession h) (hEmit h)
        atOpen <- fakeMasterGain (hFake h)
        _ <- setVolume (hSession h) (hEmit h) 0.5
        later <- fakeMasterGain (hFake h)
        bad <- mapM (setVolume (hSession h) (hEmit h)) [-0.1, 1.1, 0 / 0]
        es <- events h
        return $ either (const Nothing) (num ["volume"]) before == Just 0.25
            && atOpen == 0.25 && later == 0.5 && all ((== InvalidValue) . code) bad
            && length (named "render.started" es) == 1)

    , ("Losing the device keeps position, loop, and volume for the next play", do
        opened <- newIORef []
        eventLog <- newIORef []
        let cfg = defaultSessionConfig
                { sessionSampleRate = 1000
                , sessionRenderer = fakeRenderer fakes
                , sessionOpenAudio = \_ -> do
                    fake <- newFakeAudio 1000
                    modifyIORef' opened (fake :)
                    return (Right (fakeBackend fake))
                , sessionInitialScore = scoreA
                , sessionTelemetryMicros = 10000 }
        s <- newSession cfg
        let emit e = atomicModifyIORef' eventLog (\es -> (e : es, ()))
            h = Harness s emit eventLog (error "no shared fake") opens0
            opens0 = error "unused"
        (do _ <- startRender s emit
            _ <- waitUntil h (finishedCount 1)
            _ <- setVolume s emit 0.5
            _ <- setLoop s emit (Just (1, 3))
            _ <- play s emit
            readIORef opened >>= mapM_ (`fakeAdvance` 1234)
            readIORef opened >>= mapM_ fakeLoseDevice
            told <- waitUntil h (any (\v -> path ["deviceLost"] v == Just (Bool True)) . named "transport")
            lost <- transportView s
            firsts <- readIORef opened
            closed <- mapM fakeIsClosed firsts
            _ <- play s emit
            fakesNow <- readIORef opened
            case fakesNow of
                (f2 : _) -> do
                    st <- backendStatus (fakeBackend f2) SongVoice
                    vol <- fakeMasterGain f2
                    fakeAdvance f2 1800
                    looped <- backendStatus (fakeBackend f2) SongVoice
                    again <- transportView s
                    return $ told && and closed && length fakesNow == 2
                        && path ["playing"] lost == Just (Bool False) && num ["positionSeconds"] lost == Just 1.234
                        && statusPosition st == 1234 && statusPlaying st && vol == 0.5
                        && statusPosition looped == 1034
                        && path ["deviceLost"] again == Just (Bool False)
                [] -> return False) `finally` closeSession s)

    , ("Repeated seeks while playing end exactly at the last target", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play (hSession h) (hEmit h)
        mapM_ (\i -> seek (hSession h) (hEmit h) (fromIntegral (i * 7 `mod` 40 :: Int) / 10)
                        >> fakeAdvance (hFake h) 3) [1 .. 500]
        _ <- seek (hSession h) (hEmit h) 2
        fakeAdvance (hFake h) 10
        st <- backendStatus (fakeBackend (hFake h)) SongVoice
        opens <- readIORef (hOpens h)
        return (st == VoiceStatus 2010 4000 (statusEndCount st) True && opens == 1))

    , ("Waveform overview is bounded and tagged", withHarness fakes $ \h -> do
        early <- renderPeaks (hSession h) 100
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        r <- renderPeaks (hSession h) 100
        bad <- mapM (renderPeaks (hSession h)) [0, maxPeakBuckets + 1]
        return $ code early == NoRender && all ((== InvalidValue) . code) bad
            && either (const False) (\v -> num ["jobId"] v == Just 1
                && maybe False (\p -> case p of { Array a -> length a == 100; _ -> False }) (path ["peaks"] v)) r)

    , ("Every state-changing method is serial", withHarness fakes $ \h ->
        return (Map.keysSet (applicationMethods (hSession h)) == Set.insert "render.peaks" serialMethods))

    , ("Project commands report structured errors and re-render on success", withHarness fakes $ \h -> do
        _ <- startRender (hSession h) (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        bad <- applyProjectCommand (hSession h) (hEmit h) (UpdateTrackMix (TrackId "t1") noMixUpdate { updateGain = Just 9 })
        unknown <- applyProjectCommand (hSession h) (hEmit h) (UpdateTrackMix (TrackId "t9") noMixUpdate)
        good <- applyProjectCommand (hSession h) (hEmit h) (UpdateTrackMix (TrackId "t1") noMixUpdate { updateMute = Just True })
        rendered <- waitUntil h (finishedCount 2)
        return $ either errorCode (const InternalError) bad == InvalidValue
            && fmap (const ()) (either errorData (const Nothing) bad) == Just ()
            && either errorCode (const InternalError) unknown == UnknownTrack
            && either (const False) (const True) good && rendered)
    ]

sessionTests :: IO Bool
sessionTests = do
    putStrLn "--- Running Session (render and playback) Tests ---"
    rs <- forM scenarios (uncurry check)
    return (and rs)
