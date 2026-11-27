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
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
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
import Sonora.Project.Render (Stems, renderCompiled, renderProject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Sonora.Project.Types (Command (..), MixUpdate (..), Revision (..), TrackId (..), noMixUpdate)
import Sonora.Project.Update (newProject)
import Sonora.Render.Engine (Song (..), normalizeSignal, renderTrack)
import Sonora.Service.Methods (applicationMethods, serialMethods)
import Sonora.Service.Protocol
import Sonora.Service.Session
import Sonora.Generate.Score (GenSettings (..))
import Sonora.Audio.Audition (AuditionNote (..), holdSeconds)
import Sonora.Synth.Patch (Patch (..), defaultPatch, lookupPresetPatch)
import Sonora.Synth.Voice (VoiceConfig (..))
import Sonora.Signal.Core

-------------------------------------------------------------------------------
-- Harness

data Harness = Harness
    { hSession :: Session
    , hEmit    :: Emit
    , hEvents  :: IORef [Event]          -- newest first
    , hFake    :: FakeAudio
    , hOpens   :: IORef Int
    , hNotes   :: IORef [(Patch, AuditionNote)]  -- auditions rendered, newest first
    }

data Fakes = Fakes
    { renderDelayMs :: Int
    , renderFails   :: Bool
    , audioFails    :: Bool
    , auditionDelayMs :: Int
    , realRenderAt  :: Maybe Int  -- ^ Render for real at this sample rate.
    }

fakes :: Fakes
fakes = Fakes 0 False False 0 Nothing

-- | A fresh, empty folder for files a test writes.
scratchDir :: String -> IO FilePath
scratchDir name = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> ("sonora-test-" ++ name)
    exists <- doesDirectoryExist dir
    when exists (removeDirectoryRecursive dir)
    createDirectoryIfMissing True dir
    return dir

data Wav = Wav { wavFormat :: Int, wavChannels :: Int, wavRate :: Int, wavBits :: Int, wavData :: BS.ByteString }

readWav :: FilePath -> IO (Maybe Wav)
readWav file = do
    raw <- BS.readFile file
    let u16 at = fromIntegral (BS.index raw at) + 256 * fromIntegral (BS.index raw (at + 1)) :: Int
        u32 at = u16 at + 65536 * u16 (at + 2)
        tag at t = BS.take 4 (BS.drop at raw) == BC.pack t
    return $ if BS.length raw >= 44 && tag 0 "RIFF" && tag 8 "WAVE" && tag 12 "fmt " && tag 36 "data"
                  && u32 4 == BS.length raw - 8 && u32 40 == BS.length raw - 44
        then Just (Wav (u16 20) (u16 22) (u32 24) (u16 34) (BS.drop 44 raw))
        else Nothing

floats :: BS.ByteString -> [Float]
floats bs
    | BS.length bs < 4 = []
    | otherwise =
        let w = foldr (\i acc -> acc * 256 + fromIntegral (BS.index bs i)) 0 [0 .. 3] :: Word32
        in castWord32ToFloat w : floats (BS.drop 4 bs)

exportEvents :: Text -> Harness -> IO [Value]
exportEvents n h = named n <$> events h

-- | Audition buffers whose length identifies the request: the cutoff in
-- frames plus the gate in milliseconds.
fakeAudition :: Fakes -> IORef [(Patch, AuditionNote)] -> Int -> Patch -> AuditionNote -> IO (V.Vector Frame)
fakeAudition f notes _ p n = do
    atomicModifyIORef' notes (\ns -> ((p, n) : ns, ()))
    when (auditionDelayMs f > 0) $ threadDelay (auditionDelayMs f * 1000)
    return (V.replicate (auditionLength p n) (Frame 0.1 0.1))

auditionLength :: Patch -> AuditionNote -> Int
auditionLength p n = round (voiceFilterCutoff (patchVoice p)) + round (noteGate n * 1000)

withCutoff :: Double -> Patch
withCutoff c = (lookupPresetPatchOr "lead") { patchVoice = (patchVoice (lookupPresetPatchOr "lead")) { voiceFilterCutoff = c } }
  where lookupPresetPatchOr name = maybe defaultPatch id (lookupPresetPatch name)

-- | Frames whose value identifies the revision they were rendered from.
fakeRenderer :: Fakes -> Int -> (Int -> Int -> IO ()) -> Stems -> CompiledProject -> IO (Stems, V.Vector Frame)
fakeRenderer f _ progress stems c = fmap ((,) stems) $ do
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
    notes <- newIORef []
    fake <- newFakeAudio 1000
    let cfg = defaultSessionConfig
            { sessionSampleRate      = maybe 1000 id (realRenderAt f)
            , sessionRenderer        = maybe (fakeRenderer f) (const renderProject) (realRenderAt f)
            , sessionOpenAudio       = \_ -> do
                modifyIORef' opens (+ 1)
                return $ if audioFails f then Left (AudioError 1 "no output device") else Right (fakeBackend fake)
            , sessionInitialScore    = scoreA
            , sessionTelemetryMicros = 20000
            , sessionAuditionRenderer = fakeAudition f notes
            }
    s <- newSession cfg
    let emit e = atomicModifyIORef' eventLog (\es -> (e : es, ()))
    act (Harness s emit eventLog fake opens notes) `finally` closeSession s

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

lengthOf :: Value -> Maybe Double
lengthOf v = fmap (* 1000) (num ["seconds"] v)

-- | Run with a harness and return a value.
withHarnessV :: Fakes -> (Harness -> IO a) -> IO a
withHarnessV f act = do
    ref <- newIORef Nothing
    _ <- withHarness f (\h -> act h >>= writeIORef ref . Just >> return True)
    maybe (fail "harness failed") return =<< readIORef ref

scoreThree :: Text
scoreThree = "track lead (lead):\n    C4 quarter\ntrack bass (bass):\n    C2 half\n"

trackIdNamed :: Text -> Value -> TrackId
trackIdNamed name v = case [ TrackId i | Just ts <- [arr ["tracks"] v], t <- ts, str ["name"] t == Just name, Just i <- [str ["id"] t] ] of
    (i : _) -> i
    [] -> error "no such track"

arr :: [Text] -> Value -> Maybe [Value]
arr ks v = case path ks v of
    Just (Array a) -> Just (foldr (:) [] a)
    _ -> Nothing

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
    [ ("Renderer gives the tracks' sum normalised to 0.95, bit for bit, and reports progress", do
        txt <- TIO.readFile "examples/synthwave.sonora"
        steps <- newIORef []
        let compiled = fromJust (compileProject (newProject txt))
        out <- renderCompiled 8000 (\d t -> modifyIORef' steps ((d, t) :)) compiled
        progress <- reverse <$> readIORef steps
        return $ case parseSonora "x" (T.unpack txt) of
            Left _ -> False
            Right ast ->
                let song = evaluateSong ast
                    tracks = map (renderTrack (songBPM song)) (songTracks song)
                in out == runSignal (normalizeSignal 0.95 (foldl mixSignals emptySignal tracks)) (AudioConfig 8000 2)
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
            h = Harness s emit eventLog (error "no shared fake") opens0 (error "no notes")
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

    , ("Generating keeps the replaced score and the result as versions", withHarness fakes $ \h -> do
        let s = hSession h
            g = GenSettings 42 "D" "dorian" 98 4
        Right v <- generate s (hEmit h) g
        let txt = str ["scoreText"] v
            versions = maybe [] (\x -> case x of { Array a -> foldr (:) [] a; _ -> [] }) (path ["versions"] v)
            labels = mapMaybe (str ["label"]) versions
            currents = [ path ["current"] x | x <- versions ]
        rendered <- waitUntil h (finishedCount 1)
        -- the same settings again add nothing
        Right v2 <- generate s (hEmit h) g
        let count2 = maybe 0 (\x -> case x of { Array a -> length a; _ -> 0 }) (path ["versions"] v2)
        return $ fmap (T.isPrefixOf "title \"Generated: D dorian, 98 bpm, 4 bars, seed 42\"") txt == Just True
            && labels == ["Generated: D dorian, 98 bpm, 4 bars, seed 42", "Before generating"]
            && currents == [Just (Bool True), Just (Bool False)]
            && (num ["settings", "seed"] =<< listToMaybe versions) == Just 42
            && path ["settings"] (versions !! 1) == Just Null
            && rendered && count2 == 2)

    , ("Restoring brings back a version and keeps the edited text first", withHarness fakes $ \h -> do
        let s = hSession h
        Right _ <- generate s (hEmit h) (GenSettings 1 "C" "major" 120 2)
        _ <- setScore s (hEmit h) scoreB   -- an edit after generating
        Right v <- restoreVersion s (hEmit h) 1  -- "Before generating": scoreA
        let versions = maybe [] (\x -> case x of { Array a -> foldr (:) [] a; _ -> [] }) (path ["versions"] v)
        missing <- restoreVersion s (hEmit h) 99
        return $ str ["scoreText"] v == Just scoreA
            && mapMaybe (str ["label"]) versions
                == ["Before restoring before generating", "Generated: C major, 120 bpm, 2 bars, seed 1", "Before generating"]
            && str ["scoreText"] v /= Nothing
            && code missing == InvalidValue)

    , ("Invalid generator settings change nothing and name the fields", withHarness fakes $ \h -> do
        r <- generate (hSession h) (hEmit h) (GenSettings 1 "H" "major" 300 8)
        v <- projectView (hSession h)
        let fields = case r of
                Left (ServiceError InvalidValue _ (Just d)) -> case path ["params"] d of
                    Just (Array a) -> mapMaybe (str ["path"]) (foldr (:) [] a)
                    _ -> []
                _ -> []
        return (fields == ["root", "bpm"] && str ["scoreText"] v == Just scoreA))

    , ("At most thirty versions are kept, newest first", withHarness fakes $ \h -> do
        let s = hSession h
        mapM_ (\seed -> generate s (hEmit h) (GenSettings seed "C" "major" 120 1)) [1 .. 40]
        v <- projectView s
        let ids = maybe [] (\x -> case x of { Array a -> mapMaybe (num ["id"]) (foldr (:) [] a); _ -> [] }) (path ["versions"] v)
        return (length ids == maxVersions && ids == reverse [12 .. 41]))

    , ("An auditioned note plays over the song without touching it", withHarness fakes $ \h -> do
        let s = hSession h
        _ <- startRender s (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play s (hEmit h)
        fakeAdvance (hFake h) 100
        before <- backendStatus (fakeBackend (hFake h)) SongVoice
        r <- startAudition s (hEmit h) (withCutoff 500) 60 0.8
        ok <- waitUntil h (not . null . named "audition")
        a <- backendStatus (fakeBackend (hFake h)) AuditionVoice
        after <- backendStatus (fakeBackend (hFake h)) SongVoice
        return (either (const False) (\v -> num ["auditionId"] v == Just 1) r && ok
                && statusPlaying a && statusLength a == fromIntegral (500 + round (holdSeconds * 1000) :: Int)
                && statusPlaying after && statusPosition after == statusPosition before
                && statusLength after == statusLength before))

    , ("Fast changes to a sounding note end on the latest; none arrive stale", withHarness fakes { auditionDelayMs = 30 } $ \h -> do
        let s = hSession h
        Right v <- startAudition s (hEmit h) (withCutoff 100) 60 0.8
        let aid = round (fromJust (num ["auditionId"] v))
        rs <- mapM (\c -> updateAudition s (hEmit h) aid (withCutoff c) <* threadDelay 5000) [200, 300 .. 1000]
        ok <- waitUntil h (\es -> any ((== Just (1000 + holdSeconds * 1000)) . lengthOf) (named "audition" es))
        threadDelay 100000
        es <- events h
        a <- backendStatus (fakeBackend (hFake h)) AuditionVoice
        let played = mapMaybe (num ["revision"]) (named "audition" es)
        return (ok && all (either (const False) (\x -> path ["active"] x == Just (Bool True))) rs
                && statusLength a == fromIntegral (1000 + round (holdSeconds * 1000) :: Int)
                && played == [ r | r <- played, r == maximum played ] -- only the last revision played
                ))

    , ("Releasing re-renders the note with the key's real length", withHarness fakes $ \h -> do
        let s = hSession h
        Right v <- startAudition s (hEmit h) (withCutoff 100) 60 0.8
        let aid = round (fromJust (num ["auditionId"] v))
        _ <- waitUntil h (not . null . named "audition")
        fakeAdvance (hFake h) 400   -- 0.4 s of the note has played
        r <- releaseAudition s (hEmit h) aid
        _ <- waitUntil h ((>= 2) . length . named "audition")
        again <- releaseAudition s (hEmit h) aid
        gates <- map (noteGate . snd) <$> readIORef (hNotes h)
        a <- backendStatus (fakeBackend (hFake h)) AuditionVoice
        return (either (const False) (\x -> path ["active"] x == Just (Bool True)) r
                && map (\g -> round (g * 1000) :: Int) gates == [480, round (holdSeconds * 1000)]  -- play head plus the lead
                && statusLength a == 100 + 480 && statusPlaying a
                && either (const False) (const True) again
                && length gates == 2))                     -- a second release does nothing

    , ("A new note replaces the old one; stop silences it", withHarness fakes { auditionDelayMs = 40 } $ \h -> do
        let s = hSession h
        Right v1 <- startAudition s (hEmit h) (withCutoff 100) 60 0.8
        Right _ <- startAudition s (hEmit h) (withCutoff 200) 64 0.8
        _ <- waitUntil h (not . null . named "audition")
        threadDelay 80000
        es <- events h
        old <- updateAudition s (hEmit h) (round (fromJust (num ["auditionId"] v1))) (withCutoff 900)
        a <- backendStatus (fakeBackend (hFake h)) AuditionVoice
        _ <- stopAudition s (hEmit h)
        stopped <- backendStatus (fakeBackend (hFake h)) AuditionVoice
        return (mapMaybe (num ["auditionId"]) (named "audition" es) == [2]
                && either (const False) (\x -> path ["active"] x == Just (Bool False)) old
                && statusLength a == fromIntegral (200 + round (holdSeconds * 1000) :: Int)
                && not (statusPlaying stopped)))

    , ("Invalid sounds and pitches name the field at fault", withHarness fakes $ \h -> do
        let s = hSession h
        r1 <- startAudition s (hEmit h) (withCutoff 5) 60 0.8
        r2 <- startAudition s (hEmit h) (withCutoff 500) 200 1.5
        let fields r = case r of
                Left (ServiceError InvalidValue _ (Just d)) -> case path ["params"] d of
                    Just (Array a) -> mapMaybe (str ["path"]) (foldr (:) [] a)
                    _ -> []
                _ -> []
        notes <- readIORef (hNotes h)
        return (fields r1 == ["patch.filter.cutoff"] && fields r2 == ["pitch", "velocity"] && null notes))

    , ("Export writes the revision it started with, in the chosen format", withHarness fakes { renderDelayMs = 100 } $ \h -> do
        dir <- scratchDir "export"
        let s = hSession h
            out = dir </> "song.wav"
        Right r <- startExport s (hEmit h) (ExportRequest out 48000 32)
        _ <- setScore s (hEmit h) scoreB   -- edited while exporting
        done <- waitUntil h (not . null . named "export.finished")
        [fin] <- exportEvents "export.finished" h
        wav <- readWav out
        files <- listDirectory dir
        let rev = fromJust (num ["revision"] r)
        return $ done && files == ["song.wav"]
            && num ["revision"] fin == Just rev && str ["path"] fin == Just (T.pack out)
            && num ["sampleRate"] fin == Just 48000 && num ["bitDepth"] fin == Just 32 && num ["frames"] fin == Just 4000
            && case wav of
                Just w -> wavFormat w == 3 && wavChannels w == 2 && wavRate w == 48000 && wavBits w == 32
                    && BS.length (wavData w) == 4000 * 8
                    && take 2 (floats (wavData w)) == replicate 2 (realToFrac (rev / 100))
                Nothing -> False)

    , ("16 and 24-bit exports have the matching layout", withHarness fakes $ \h -> do
        dir <- scratchDir "depths"
        oks <- forM [(16, 4), (24, 6)] $ \(bits, bytes) -> do
            let out = dir </> ("d" ++ show bits ++ ".wav")
            Right _ <- startExport (hSession h) (hEmit h) (ExportRequest out 44100 bits)
            _ <- waitUntil h ((>= (bits `div` 8 - 1)) . length . named "export.finished")
            w <- readWav out
            return $ maybe False (\x -> wavFormat x == 1 && wavBits x == bits && wavRate x == 44100
                && BS.length (wavData x) == 4000 * bytes) w
        return (and oks))

    , ("Cancelling an export leaves no file behind", withHarness fakes { renderDelayMs = 400 } $ \h -> do
        dir <- scratchDir "cancel"
        let s = hSession h
        Right _ <- startExport s (hEmit h) (ExportRequest (dir </> "gone.wav") 44100 24)
        busy <- startExport s (hEmit h) (ExportRequest (dir </> "other.wav") 44100 24)
        threadDelay 50000
        c1 <- cancelExport s (hEmit h)
        c2 <- cancelExport s (hEmit h)
        threadDelay 500000
        files <- listDirectory dir
        es <- events h
        return (null files && path ["cancelled"] c1 == Just (Bool True) && path ["cancelled"] c2 == Just (Bool False)
                && length (named "export.cancelled" es) == 1 && null (named "export.finished" es)
                && code busy == InvalidValue))

    , ("Export problems say which setting is wrong, or what to do", withHarness fakes $ \h -> do
        dir <- scratchDir "problems"
        let s = hSession h
            fields r = case r of
                Left (ServiceError InvalidValue _ (Just d)) -> case path ["params"] d of
                    Just (Array a) -> mapMaybe (str ["path"]) (foldr (:) [] a)
                    _ -> []
                _ -> []
        r1 <- startExport s (hEmit h) (ExportRequest (dir </> "song.mp3") 22050 8)
        r2 <- startExport s (hEmit h) (ExportRequest (dir </> "missing" </> "song.wav") 44100 16)
        -- A folder with the file's name is refused at once.
        createDirectoryIfMissing True (dir </> "taken.wav")
        r3 <- startExport s (hEmit h) (ExportRequest (dir </> "taken.wav") 44100 16)
        -- A folder where the temporary file goes: the write fails after rendering.
        createDirectoryIfMissing True (dir </> "late.wav.part")
        Right _ <- startExport s (hEmit h) (ExportRequest (dir </> "late.wav") 44100 16)
        _ <- waitUntil h (not . null . named "export.failed")
        [failed] <- exportEvents "export.failed" h
        final <- doesFileExist (dir </> "late.wav")
        let message = maybe "" id (str ["message"] failed)
        return (fields r1 == ["path", "sampleRate", "bitDepth"] && fields r2 == ["path"] && fields r3 == ["path"]
                && "late.wav" `T.isInfixOf` message && "Choose" `T.isInfixOf` message
                && not ("backtrace" `T.isInfixOf` T.toLower message) && not final))

    , ("The exported file is exactly what plays", withHarness fakes { realRenderAt = Just 44100 } $ \h -> do
        dir <- scratchDir "same"
        let s = hSession h
            out = dir </> "same.wav"
        _ <- startRender s (hEmit h)
        _ <- waitUntil h (finishedCount 1)
        _ <- play s (hEmit h)
        playing <- fakeFrames (hFake h) SongVoice
        Right _ <- startExport s (hEmit h) (ExportRequest out 44100 32)
        _ <- waitUntil h (not . null . named "export.finished")
        w <- readWav out
        let expected = concat [ [realToFrac l, realToFrac r] | Frame l r <- V.toList playing ] :: [Float]
        return (V.length playing > 1000 && maybe False ((== expected) . floats . wavData) w))

    , ("A saved project reopens as it was, in a new session", do
        dir <- scratchDir "save"
        let file = dir </> "song.sonoraproj"
        (saved, untitled, afterSave, edited) <- withHarnessV fakes $ \h -> do
            let s = hSession h
            _ <- setScore s (hEmit h) scoreThree
            v0 <- projectView s
            let lead = trackIdNamed "lead" v0
            Right _ <- applyProjectCommand s (hEmit h) (UpdateTrackMix lead noMixUpdate { updateGain = Just 0.5, updateSolo = Just True })
            Right _ <- generate s (hEmit h) (GenSettings 3 "A" "dorian" 90 2)
            Right _ <- restoreVersion s (hEmit h) 1
            before <- projectView s
            r <- saveProject s (Just file)
            v1 <- either (const (return Null)) return r
            _ <- setScore s (hEmit h) (scoreThree <> "\n")
            v2 <- projectView s
            return (before, path ["file", "modified"] before, path ["file", "modified"] v1, path ["file", "modified"] v2)
        withHarness fakes $ \h -> do
            r <- openProject (hSession h) (hEmit h) file
            ok <- waitUntil h (finishedCount 1)
            let opened = either (const Null) id r
                same k = path [k] opened == path [k] saved
            return (ok && all same ["scoreText", "tracks", "master", "song"]
                    && fmap (map (str ["label"])) (arr ["versions"] opened) == fmap (map (str ["label"])) (arr ["versions"] saved)
                    && str ["file", "name"] opened == Just "song.sonoraproj"
                    && path ["file", "modified"] opened == Just (Bool False)
                    && untitled == Just (Bool True) && afterSave == Just (Bool False) && edited == Just (Bool True)
                    && num ["revision"] opened > Just 0))

    , ("Opening a damaged or newer file changes nothing", withHarness fakes $ \h -> do
        dir <- scratchDir "bad"
        let s = hSession h
        BL.writeFile (dir </> "newer.sonoraproj") "{\"format\":\"sonora-project\",\"version\":99}"
        BL.writeFile (dir </> "junk.sonoraproj") "not a project"
        before <- projectView s
        r1 <- openProject s (hEmit h) (dir </> "newer.sonoraproj")
        r2 <- openProject s (hEmit h) (dir </> "junk.sonoraproj")
        r3 <- openProject s (hEmit h) (dir </> "missing.sonoraproj")
        after <- projectView s
        return (code r1 == InvalidProject && either (T.isInfixOf "newer version" . errorMessage) (const False) r1
                && code r2 == InvalidProject && code r3 == FileError
                && path ["revision"] after == path ["revision"] before && path ["scoreText"] after == path ["scoreText"] before))

    , ("Saving checks the target and needs a path the first time", withHarness fakes $ \h -> do
        dir <- scratchDir "target"
        let s = hSession h
        r1 <- saveProject s Nothing
        r2 <- saveProject s (Just (dir </> "song.txt"))
        createDirectoryIfMissing True (dir </> "taken.sonoraproj")
        r3 <- saveProject s (Just (dir </> "taken.sonoraproj"))
        files <- listDirectory dir
        return (code r1 == InvalidValue && code r2 == InvalidValue && code r3 == InvalidValue && files == ["taken.sonoraproj"]))

    , ("Scores import and export as plain text, separate from projects", withHarness fakes $ \h -> do
        dir <- scratchDir "score"
        let s = hSession h
            out = dir </> "mine.sonora"
        _ <- setScore s (hEmit h) scoreB
        Right _ <- exportScore s out
        written <- BS.readFile out
        BS.writeFile (dir </> "theirs.sonora") ("\xEF\xBB\xBF" <> encodeUtf8 scoreC)
        r <- importScore s (hEmit h) (dir </> "theirs.sonora")
        BS.writeFile (dir </> "binary.sonora") (BS.pack [0xff, 0xfe, 0x00, 0xd8])
        bad <- importScore s (hEmit h) (dir </> "binary.sonora")
        notScore <- exportScore s (dir </> "mine.wav")
        let v = either (const Null) id r
        return (written == encodeUtf8 scoreB
                && str ["scoreText"] v == Just scoreC
                && fmap (take 1 . map (str ["label"])) (arr ["versions"] v) == Just [Just "Before importing theirs.sonora"]
                && code bad == FileError && code notScore == InvalidValue))

    , ("New starts from the starter score, untitled", withHarness fakes $ \h -> do
        let s = hSession h
        _ <- setScore s (hEmit h) scoreB
        v <- startNewProject s (hEmit h)
        return (str ["scoreText"] v == Just scoreA && path ["file", "path"] v == Just Null
                && path ["file", "modified"] v == Just (Bool False) && arr ["versions"] v == Just []))

    , ("Every state-changing method is serial", withHarness fakes $ \h ->
        return (Map.keysSet (applicationMethods (hSession h)) == Set.insert "project.timing" (Set.insert "render.peaks" serialMethods)))

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
