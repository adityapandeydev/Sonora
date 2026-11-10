-- | Deterministic in-memory audio backend for tests.
--
-- Nothing is played; time only moves when 'fakeAdvance' is called, and each
-- voice follows the reference model in "Sonora.Audio.Backend".
module Sonora.Audio.Fake
    ( FakeAudio (..)
    , newFakeAudio
    ) where

import Data.IORef
import Data.Word (Word64)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Signal.Core (Frame)

data FakeVoice = FakeVoice
    { fvModel  :: !VoiceModel
    , fvFrames :: !(V.Vector Frame)
    , fvGain   :: !Double
    }

data FakeState = FakeState
    { fsVoices     :: !(Map.Map Voice FakeVoice)
    , fsMasterGain :: !Double
    , fsLost       :: !Bool
    , fsClosed     :: !Bool
    }

data FakeAudio = FakeAudio
    { fakeBackend    :: AudioBackend
      -- | Advance the output clock by a number of frames.
    , fakeAdvance    :: Word64 -> IO ()
      -- | Buffer currently loaded on a voice.
    , fakeFrames     :: Voice -> IO (V.Vector Frame)
    , fakeGain       :: Voice -> IO Double
    , fakeMasterGain :: IO Double
      -- | Simulate the output device disappearing.
    , fakeLoseDevice :: IO ()
    , fakeIsClosed   :: IO Bool
    }

newFakeAudio :: Int -> IO FakeAudio
newFakeAudio sampleRate = do
    ref <- newIORef FakeState
        { fsVoices     = Map.fromList [ (v, FakeVoice emptyVoiceModel V.empty 1) | v <- [minBound .. maxBound] ]
        , fsMasterGain = 1
        , fsLost       = False
        , fsClosed     = False
        }
    let modifyOpen f = atomicModifyIORef' ref $ \s ->
            (if fsClosed s then s else f s, ())
        onVoice v f = modifyOpen $ \s -> s { fsVoices = Map.adjust f v (fsVoices s) }
        onModel v f = onVoice v (\fv -> fv { fvModel = f (fvModel fv) })
        voice v = (Map.! v) . fsVoices <$> readIORef ref
        backend = AudioBackend
            { backendDescription   = "fake"
            , backendSampleRate    = sampleRate
            , backendLoad          = \v frames -> onVoice v $ \fv ->
                fv { fvFrames = frames
                   , fvModel  = loadVoiceModel (fromIntegral (V.length frames)) (fvModel fv)
                   }
            , backendSetPlaying    = \v p -> onModel v (setPlayingModel p)
            , backendSeek          = \v pos -> onModel v (seekModel pos)
            , backendSetLoop       = \v l -> onModel v (setLoopModel l)
            , backendSetGain       = \v g -> onVoice v (\fv -> fv { fvGain = max 0 g })
            , backendSetMasterGain = \g -> modifyOpen (\s -> s { fsMasterGain = max 0 g })
            , backendStatus        = \v -> do
                s <- readIORef ref
                return $ if fsClosed s
                    then voiceModelStatus emptyVoiceModel
                    else voiceModelStatus (fvModel (fsVoices s Map.! v))
            , backendDeviceLost    = fsLost <$> readIORef ref
            , backendClose         = atomicModifyIORef' ref (\s -> (s { fsClosed = True }, ()))
            }
    return FakeAudio
        { fakeBackend    = backend
        , fakeAdvance    = \n -> modifyOpen $ \s ->
            if fsLost s then s
            else s { fsVoices = Map.map (\fv -> fv { fvModel = advanceVoice n (fvModel fv) }) (fsVoices s) }
        , fakeFrames     = fmap fvFrames . voice
        , fakeGain       = fmap fvGain . voice
        , fakeMasterGain = fsMasterGain <$> readIORef ref
          -- Like a real device, a lost one stops consuming frames but leaves
          -- voice state untouched.
        , fakeLoseDevice = modifyOpen $ \s -> s { fsLost = True }
        , fakeIsClosed   = fsClosed <$> readIORef ref
        }
