module AudioSpec (audioTests) where

import Control.Monad (forM)
import Data.Word (Word32)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Audio.Device (simulateNativeVoice)
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

-- | The native realtime callback follows the reference model for any
-- sequence of device period sizes.
prop_nativeMatchesModel :: Property
prop_nativeMatchesModel =
    forAll genVoiceModel $ \vm ->
    forAll genChunks $ \chunks -> monadicIO $ do
        native <- run (simulateNativeVoice vm chunks)
        let model = foldl (\m c -> advanceVoice (fromIntegral c) m) vm chunks
        assert (native == voiceModelStatus model)

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
    ]

audioTests :: IO Bool
audioTests = do
    putStrLn "--- Running Audio Transport Tests ---"
    rs <- forM [prop_advanceAdditive, prop_positionBounded, prop_nativeMatchesModel] (quickCheckWithResult stdArgs { maxSuccess = 500 })
    interleaved <- check "Frames interleave as L,R,L,R"
        (framesToInterleaved (V.fromList [Frame 1 2, Frame 3 4]) == V.fromList [1, 2, 3, 4])
    fakes <- fakeScenarios
    return (all isSuccess rs && interleaved && and fakes)
