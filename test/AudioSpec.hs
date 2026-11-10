module AudioSpec (audioTests) where

import Control.Monad (forM)
import Data.Word (Word32)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Audio.Fake
import Sonora.Signal.Core (Frame (..))

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
            sample i = fromIntegral i / 1024 :: Double
            buffer = V.generate len (\i -> Frame (sample i) (negate (sample i)))
        (out, st) <- run $ do
            fake <- newFakeAudio 44100
            let b = fakeBackend fake
            backendLoad b SongVoice buffer
            backendSetLoop b SongVoice (vmLoop vm)
            backendSeek b SongVoice (vmPosition vm)
            backendSetPlaying b SongVoice (vmPlaying vm)
            blocks <- mapM (fakeRender fake . fromIntegral) chunks
            st <- backendStatus b SongVoice
            return (V.concat blocks, st)
        let total = sum (map fromIntegral chunks) :: Int
            models = take total (iterate (advanceVoice 1) vm)
            expected = V.fromList $ concatMap
                (\m -> if vmPlaying m
                          then let x = realToFrac (sample (fromIntegral (vmPosition m))) in [x, negate x]
                          else [0, 0])
                models
            final = foldl (\m c -> advanceVoice (fromIntegral c) m) vm chunks
        assert (out == expected && st == voiceModelStatus final)

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
    ]

audioTests :: IO Bool
audioTests = do
    putStrLn "--- Running Audio Transport Tests ---"
    rs <- forM [prop_advanceAdditive, prop_positionBounded, prop_mixerMatchesModel] (quickCheckWithResult stdArgs { maxSuccess = 500 })
    interleaved <- check "Frames interleave as L,R,L,R"
        (framesToInterleaved (V.fromList [Frame 1 2, Frame 3 4]) == V.fromList [1, 2, 3, 4])
    fakes <- fakeScenarios
    return (all isSuccess rs && interleaved && and fakes)
