-- | Deterministic audio backend for tests.
--
-- Drives the real "Sonora.Audio.Mixer" from a manual clock instead of a
-- device: time only moves when 'fakeAdvance' or 'fakeRender' is called.
module Sonora.Audio.Fake
    ( FakeAudio (..)
    , newFakeAudio
    ) where

import Control.Monad (unless)
import Data.IORef
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Sonora.Audio.Backend
import Sonora.Audio.Mixer
import Sonora.Signal.Core (Frame)

data FakeAudio = FakeAudio
    { fakeBackend    :: AudioBackend
      -- | Render and discard a number of frames.
    , fakeAdvance    :: Int -> IO ()
      -- | Render a number of frames and return them interleaved (L, R, ...).
      -- Returns silence without advancing once the device is lost or closed.
    , fakeRender     :: Int -> IO (V.Vector Float)
    , fakeFrames     :: Voice -> IO (V.Vector Frame)
    , fakeGain       :: Voice -> IO Double
    , fakeMasterGain :: IO Double
      -- | Simulate the output device disappearing.
    , fakeLoseDevice :: IO ()
    , fakeIsClosed   :: IO Bool
    }

newFakeAudio :: Int -> IO FakeAudio
newFakeAudio sampleRate = do
    mixer <- newMixer sampleRate
    lost <- newIORef False
    closed <- newIORef False
    backend <- mixerBackend mixer "fake" 0 (readIORef lost) (return 0) (writeIORef closed True)
    let render n = do
            l <- readIORef lost
            c <- readIORef closed
            buf <- MV.replicate (2 * max 0 n) 0
            unless (l || c || n <= 0) $
                MV.unsafeWith buf (mixerRender mixer n)
            V.freeze buf
    return FakeAudio
        { fakeBackend    = backend
        , fakeAdvance    = \n -> () <$ render n
        , fakeRender     = render
        , fakeFrames     = mixerFrames mixer
        , fakeGain       = mixerTargetGain mixer
        , fakeMasterGain = mixerMasterGain mixer
        , fakeLoseDevice = writeIORef lost True
        , fakeIsClosed   = readIORef closed
        }
