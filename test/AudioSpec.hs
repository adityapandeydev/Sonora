module AudioSpec (audioTests) where

import Control.Monad (forM)
import Data.Word (Word32, Word64)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Audio.Fake
import Sonora.Signal.Core (Frame (..))
import Sonora.Signal.Waveform (waveformPeaks)

-- | Voice states reachable through the public operations, including loop
-- ranges that are inverted or extend past the buffer.
genVoiceModel :: Gen VoiceModel
genVoiceModel = do
    len <- fromIntegral <$> choose (0 :: Int, 400)
    pos <- fromIntegral <$> choose (0 :: Int, fromIntegral len)
    playing <- arbitrary
    loopRange <- oneof
        [ return Nothing
        , do s <- fromIntegral <$> choose (0 :: Int, fromIntegral len + 20)
             e <- fromIntegral <$> choose (0 :: Int, fromIntegral len + 20)
             return (Just (s, e))
        ]
    return $ setPlayingModel playing
           $ setLoopModel loopRange
           $ seekModel pos
           $ loadVoiceModel len emptyVoiceModel

genChunks :: Gen [Word32]
genChunks = listOf1 (fromIntegral <$> choose (1 :: Int, 300))

-- | Advancing in two steps is the same as advancing once by the sum.
prop_advanceAdditive :: Property
prop_advanceAdditive =
    forAll genVoiceModel $ \vm ->
    forAll (choose (0, 900)) $ \a ->
    forAll (choose (0, 900)) $ \b ->
        advanceVoice (a + b) vm === advanceVoice b (advanceVoice a vm)

-- | The position never leaves the buffer.
prop_positionBounded :: Property
prop_positionBounded =
    forAll genVoiceModel $ \vm ->
    forAll (choose (0, 5000)) $ \n ->
        let vm' = advanceVoice n vm in vmPosition vm' <= vmLength vm'

-- | The mixer's rendered samples follow the reference model frame by frame
-- for any sequence of device period sizes, and its final state matches.
-- Each buffer frame encodes its own index so the output shows exactly which
-- frame was played (gains are 1, so samples pass through unchanged).
prop_mixerMatchesModel :: Property
prop_mixerMatchesModel =
    forAll genVoiceModel $ \vm ->
    forAll genChunks $ \chunks -> monadicIO $ do
        let len = fromIntegral (vmLength vm) :: Int
            sampleAt :: Int -> Double
            sampleAt i = fromIntegral i / 1024
            buffer = V.generate len (\i -> Frame (sampleAt i) (negate (sampleAt i)))
        (out, st) <- run $ do
            fake <- newFakeAudioWith 44100 0 0  -- fades off: samples pass through exactly
            let b = fakeBackend fake
            backendLoad b SongVoice buffer
            backendSetLoop b SongVoice (vmLoop vm)
            backendSeek b SongVoice (vmPosition vm)
            backendSetPlaying b SongVoice (vmPlaying vm)
            blocks <- mapM (fakeRender fake . fromIntegral) chunks
            st <- backendStatus b SongVoice
            return (V.concat blocks, st)
        let frameTotal = sum (map fromIntegral chunks) :: Int
            models = take frameTotal (iterate (advanceVoice 1) vm)
            expected = V.fromList $ concatMap
                (\m -> if vmPlaying m
                          then let x = realToFrac (sampleAt (fromIntegral (vmPosition m))) in [x, negate x]
                          else [0, 0])
                models
            final = foldl (\m c -> advanceVoice (fromIntegral c) m) vm chunks
        assert (out == expected && st == voiceModelStatus final)

-- | Control operations for 'prop_heardPosition'.
data Op = OpPlay Bool | OpSeek Word64 | OpLoop (Maybe (Word64, Word64)) | OpReplace Word64
    deriving Show

genOp :: Gen Op
genOp = oneof
    [ OpPlay <$> arbitrary
    , OpSeek <$> pos
    , OpLoop <$> oneof [return Nothing, Just <$> ((,) <$> pos <*> pos)]
    , OpReplace <$> pos ]
  where pos = fromIntegral <$> choose (0 :: Int, 420)

-- | After any sequence of operations and block sizes, the heard position is
-- the one a frame-by-frame reference put at the output frame the device is
-- playing, or the target of a load or seek that is not audible yet.
prop_heardPosition :: Property
prop_heardPosition =
    forAll (choose (1, 500)) $ \latency ->
    forAll (listOf1 ((,) <$> genOp <*> choose (1 :: Int, 300))) $ \steps -> monadicIO $ do
        ok <- run $ do
            fake <- newFakeAudioWith 44100 8 latency
            let b = fakeBackend fake
            backendLoad b SongVoice (silence 400)
            backendSetPlaying b SongVoice True
            let start = setPlayingModel True (loadVoiceModel 400 emptyVoiceModel)
                -- Model, generation and its start, and what each rendered
                -- frame was (newest first).
                loop _ [] = return True
                loop (m, gen, genStart, trace) ((op, n) : rest) = do
                    (m', gen', genStart') <- case op of
                        OpPlay p -> do
                            backendSetPlaying b SongVoice p
                            return (setPlayingModel p m, gen, genStart)
                        OpSeek p -> do
                            backendSeek b SongVoice p
                            let m1 = seekModel p m
                            return (m1, gen + 1 :: Int, vmPosition m1)
                        OpLoop l -> do
                            backendSetLoop b SongVoice l
                            return (setLoopModel l m, gen, genStart)
                        OpReplace len -> do
                            backendReplace b SongVoice (silence (fromIntegral len))
                            return (replaceVoiceModel len m, gen, genStart)
                    fakeAdvance fake n
                    let models = take n (iterate (advanceVoice 1) m')
                        trace' = reverse [ (gen', vmPosition x) | x <- models ] ++ trace
                        final = advanceVoice (fromIntegral n) m'
                        played = max 0 (length trace' - latency)
                        expected = min (vmLength final) $ case drop (length trace' - 1 - played) trace' of
                            (g, p) : _ | g == gen' -> p
                            _ -> genStart'
                    heard <- backendHeard b SongVoice
                    status <- backendStatus b SongVoice
                    if heardPosition heard == expected && statusPosition status == vmPosition final
                        then loop (final, gen', genStart', trace') rest
                        else return False
            loop (start, 0, 0, []) steps
        assert ok

silence :: Int -> V.Vector Frame
silence n = V.replicate n (Frame 0 0)

check :: String -> Bool -> IO Bool
check name ok = do
    putStrLn $ "  " ++ name ++ ": " ++ (if ok then "PASSED" else "FAILED")
    return ok

-- | Run a scenario against a fresh fake backend and return the song status.
scenario :: (FakeAudio -> AudioBackend -> IO a) -> IO a
scenario f = do
    fake <- newFakeAudio 44100
    f fake (fakeBackend fake)

fakeScenarios :: IO [Bool]
fakeScenarios = sequence
    [ scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendSetPlaying b SongVoice True
        fakeAdvance fake 60
        mid <- backendStatus b SongVoice
        fakeAdvance fake 60
        end <- backendStatus b SongVoice
        check "Song stops once at end of buffer" $
            mid == VoiceStatus 60 100 0 True && end == VoiceStatus 100 100 1 False

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendSetLoop b SongVoice (Just (20, 50))
        backendSeek b SongVoice 10
        backendSetPlaying b SongVoice True
        fakeAdvance fake 45
        st <- backendStatus b SongVoice
        check "Loop wraps at loop end" (statusPosition st == 25 && statusPlaying st)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendSetPlaying b SongVoice True
        fakeAdvance fake 30
        backendLoad b AuditionVoice (silence 10)
        backendSetPlaying b AuditionVoice True
        fakeAdvance fake 20
        song <- backendStatus b SongVoice
        aud <- backendStatus b AuditionVoice
        frames <- fakeFrames fake SongVoice
        check "Audition leaves song buffer and position alone" $
            statusPosition song == 50 && statusPlaying song && V.length frames == 100
            && aud == VoiceStatus 10 10 1 False

    , scenario $ \_ b -> do
        backendLoad b SongVoice (silence 100)
        backendSeek b SongVoice 500
        backendSetPlaying b SongVoice True
        st <- backendStatus b SongVoice
        check "Seek clamps; play at end does not start" (st == VoiceStatus 100 100 0 False)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendSetPlaying b SongVoice True
        fakeAdvance fake 40
        backendLoad b SongVoice (silence 200)
        st <- backendStatus b SongVoice
        check "Loading a new buffer stops and rewinds" (st == VoiceStatus 0 200 0 False)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendSetPlaying b SongVoice True
        fakeLoseDevice fake
        fakeAdvance fake 40
        lost <- backendDeviceLost b
        st <- backendStatus b SongVoice
        check "Lost device stops consuming frames" (lost && statusPosition st == 0)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 100)
        backendClose b
        backendSetPlaying b SongVoice True
        backendClose b
        st <- backendStatus b SongVoice
        closed <- fakeIsClosed fake
        check "Closed backend is inert and close is idempotent" (closed && st == VoiceStatus 0 0 0 False)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (V.replicate 20000 (Frame 0.5 0.5))
        backendSetPlaying b SongVoice True
        fakeAdvance fake 100
        backendSetGain b SongVoice 0
        out <- fakeRender fake 4000
        let firstL = V.head out
            lastL = out V.! (V.length out - 2)
            steps = [ abs (out V.! (2 * (i + 1)) - out V.! (2 * i)) | i <- [0 .. 3998] ]
        check "Gain changes ramp smoothly to target" $
            firstL > 0.45 && firstL < 0.5 && lastL < 0.001 && maximum steps < 0.01

    , scenario $ \fake b -> do
        backendLoad b SongVoice (V.replicate 100 (Frame 0.8 (-0.8)))
        backendLoad b AuditionVoice (V.replicate 100 (Frame 0.8 (-0.8)))
        backendSetPlaying b SongVoice True
        backendSetPlaying b AuditionVoice True
        out <- fakeRender fake 10
        check "Voices sum and output is clamped" (V.all (\x -> abs x == 1) out)

    , do
        -- Each cut jumps between +0.8 and -0.8: an unfaded cut steps by 1.6
        -- (0.8 for pause). Faded over 4 ms, no step exceeds 0.02.
        let hilo = V.generate 40000 (\i -> let x = if i < 20000 then 0.8 else -0.8 in Frame x x)
            low = V.replicate 40000 (Frame (-0.8) (-0.8))
            cuts =
                [ ("pause", \b -> backendSetPlaying b SongVoice False)
                , ("seek", \b -> backendSeek b SongVoice 30000)
                , ("load", \b -> backendLoad b SongVoice low)
                , ("replace", \b -> backendReplace b SongVoice low)
                , ("loop wrap", \b -> backendSetLoop b SongVoice (Just (25000, 35000)) >> backendSeek b SongVoice 34900)
                , ("loop wrap to other level", \b -> backendSetLoop b SongVoice (Just (5000, 15000)) >> backendSeek b SongVoice 14990)
                , ("stop", \b -> backendSetPlaying b SongVoice False >> backendSeek b SongVoice 0)
                , ("rapid seeks", \b -> mapM_ (backendSeek b SongVoice) [30000, 1000, 31000, 2000])
                ]
            biggestStep fade act = do
                fake <- newFakeAudioWith 44100 fade 0
                let b = fakeBackend fake
                backendLoad b SongVoice hilo
                backendSetPlaying b SongVoice True
                before <- fakeRender fake 1000
                _ <- act b
                after <- fakeRender fake 2000
                let xs = V.toList (V.ifilter (\i _ -> even i) (before V.++ after))
                return (maximum (zipWith (\x y -> abs (x - y)) (drop 1 xs) xs))
        faded <- mapM (\(n, act) -> (,) n <$> biggestStep 176 act) cuts
        raw <- biggestStep 0 (\b -> backendSeek b SongVoice 30000)
        let bad = [ (n, s) | (n, s) <- faded, s > 0.02 ]
        mapM_ (\(n, s) -> putStrLn ("    step at " ++ n ++ ": " ++ show s)) bad
        check "Pause, seek, load, replace, loop wrap, and stop do not click" (null bad && raw > 1.5)

    , scenario $ \fake b -> do
        backendLoad b SongVoice (silence 1000)
        backendSetLoop b SongVoice (Just (200, 400))
        backendSeek b SongVoice 300
        backendSetPlaying b SongVoice True
        fakeAdvance fake 50
        backendReplace b SongVoice (silence 800)
        kept <- backendStatus b SongVoice
        fakeAdvance fake 100
        looped <- backendStatus b SongVoice
        backendReplace b SongVoice (silence 100)
        short <- backendStatus b SongVoice
        check "Replacing a buffer keeps position, loop, and playback" $
            kept == VoiceStatus 350 800 0 True && statusPosition looped == 250 && statusPlaying looped
            && short == VoiceStatus 100 100 1 False

    , check "Waveform peaks cover every frame once" $
        waveformPeaks 3 (V.fromList [Frame 1 0, Frame 0 (-2), Frame 3 0, Frame 0 0, Frame (-4) 0, Frame 0 5, Frame 0.5 0.5])
            == [(-2, 1), (0, 3), (-4, 5)]
        && length (waveformPeaks 10 (silence 4)) == 4
        && null (waveformPeaks 10 V.empty)
    ]

audioTests :: IO Bool
audioTests = do
    putStrLn "--- Running Audio Transport Tests ---"
    rs <- forM [prop_advanceAdditive, prop_positionBounded, prop_mixerMatchesModel, prop_heardPosition] (quickCheckWithResult stdArgs { maxSuccess = 500 })
    interleaved <- check "Frames interleave as L,R,L,R"
        (framesToInterleaved (V.fromList [Frame 1 2, Frame 3 4]) == V.fromList [1, 2, 3, 4])
    fakes <- fakeScenarios
    return (all isSuccess rs && interleaved && and fakes)
