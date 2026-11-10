{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Backend-neutral audio output interface.
--
-- An 'AudioBackend' plays prepared stereo buffers on two independent voices:
-- 'SongVoice' for arrangement playback and 'AuditionVoice' for short patch
-- previews, so an audition never replaces the loaded song. Positions are
-- measured in frames of the loaded buffer at 'backendSampleRate'.
--
-- 'advanceVoice' is the reference model of how a voice moves through its
-- buffer. The native device ("Sonora.Audio.Device") and the deterministic
-- test backend ("Sonora.Audio.Fake") both follow it.
module Sonora.Audio.Backend
    ( Voice (..)
    , voiceIndex
    , VoiceStatus (..)
    , AudioError (..)
    , AudioBackend (..)
      -- * Reference transport model
    , VoiceModel (..)
    , emptyVoiceModel
    , loadVoiceModel
    , setPlayingModel
    , seekModel
    , setLoopModel
    , advanceVoice
    , voiceModelStatus
      -- * Buffer conversion
    , framesToInterleaved
    ) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core (Frame (..))

-- | Independent playback voices mixed into one output stream.
data Voice
    = SongVoice
    | AuditionVoice
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Index used by the native shim.
voiceIndex :: Voice -> Int32
voiceIndex SongVoice     = 0
voiceIndex AuditionVoice = 1

-- | Observable state of a voice.
data VoiceStatus = VoiceStatus
    { statusPosition :: !Word64  -- ^ Next frame to be played.
    , statusLength   :: !Word64  -- ^ Frames in the loaded buffer.
    , statusEndCount :: !Word32  -- ^ Times the voice reached the end of its buffer.
    , statusPlaying  :: !Bool
    } deriving (Eq, Show, Generic)

-- | An audio backend failure, such as no available output device.
data AudioError = AudioError
    { audioErrorCode    :: !Int
    , audioErrorMessage :: !String
    } deriving (Eq, Show, Generic)

-- | A running audio output.
--
-- All operations are safe to call from any Haskell thread. Operations on a
-- closed backend are no-ops, and status reports an empty voice.
data AudioBackend = AudioBackend
    { backendDescription   :: !String
    , backendSampleRate    :: !Int
      -- | Replace a voice's buffer. The voice stops, rewinds, and clears its loop.
    , backendLoad          :: Voice -> V.Vector Frame -> IO ()
    , backendSetPlaying    :: Voice -> Bool -> IO ()
      -- | Move to a frame; positions past the end clamp to the buffer length.
    , backendSeek          :: Voice -> Word64 -> IO ()
      -- | Loop the half-open frame range @[start, end)@, or disable looping.
    , backendSetLoop       :: Voice -> Maybe (Word64, Word64) -> IO ()
      -- | Linear voice gain (clamped at 0), smoothed to avoid clicks.
    , backendSetGain       :: Voice -> Double -> IO ()
    , backendSetMasterGain :: Double -> IO ()
    , backendStatus        :: Voice -> IO VoiceStatus
      -- | True once the device stopped without being asked to.
    , backendDeviceLost    :: IO Bool
    , backendClose         :: IO ()
    }

-- | Reference model of a single voice's transport state.
data VoiceModel = VoiceModel
    { vmLength   :: !Word64
    , vmPosition :: !Word64
    , vmPlaying  :: !Bool
    , vmLoop     :: !(Maybe (Word64, Word64))
    , vmEndCount :: !Word32
    } deriving (Eq, Show, Generic)

emptyVoiceModel :: VoiceModel
emptyVoiceModel = VoiceModel 0 0 False Nothing 0

-- | Loading resets position, playback, and loop; the end count is kept so
-- observers can still detect ends by comparing counts.
loadVoiceModel :: Word64 -> VoiceModel -> VoiceModel
loadVoiceModel len vm = vm { vmLength = len, vmPosition = 0, vmPlaying = False, vmLoop = Nothing }

-- | A voice only starts if it has frames left to play.
setPlayingModel :: Bool -> VoiceModel -> VoiceModel
setPlayingModel p vm = vm { vmPlaying = p && vmPosition vm < vmLength vm }

seekModel :: Word64 -> VoiceModel -> VoiceModel
seekModel pos vm = vm { vmPosition = min pos (vmLength vm) }

setLoopModel :: Maybe (Word64, Word64) -> VoiceModel -> VoiceModel
setLoopModel l vm = vm { vmLoop = l }

-- | The loop range, if it is usable for the current buffer.
activeLoop :: VoiceModel -> Maybe (Word64, Word64)
activeLoop vm = case vmLoop vm of
    Just (s, e) | e > s && e <= vmLength vm -> Just (s, e)
    _                                       -> Nothing

-- | Advance a voice by a number of output frames.
--
-- A loop wraps only when playback crosses its end; a voice positioned past
-- the loop end plays on to the end of the buffer. Reaching the end of the
-- buffer stops the voice and increments its end count.
advanceVoice :: Word64 -> VoiceModel -> VoiceModel
advanceVoice !n vm
    | not (vmPlaying vm) = vm
    | otherwise =
        case activeLoop vm of
            Just (s, e) | pos < e ->
                let toLoopEnd = e - pos
                in if n < toLoopEnd
                       then vm { vmPosition = pos + n }
                       else vm { vmPosition = s + (n - toLoopEnd) `mod` (e - s) }
            _ ->
                let toEnd = vmLength vm - pos
                in if n < toEnd
                       then vm { vmPosition = pos + n }
                       else vm { vmPosition = vmLength vm
                               , vmPlaying  = False
                               , vmEndCount = vmEndCount vm + 1
                               }
  where
    pos = vmPosition vm

voiceModelStatus :: VoiceModel -> VoiceStatus
voiceModelStatus vm = VoiceStatus
    { statusPosition = vmPosition vm
    , statusLength   = vmLength vm
    , statusEndCount = vmEndCount vm
    , statusPlaying  = vmPlaying vm
    }

-- | Interleave stereo frames as 32-bit floats (L, R, L, R, ...).
framesToInterleaved :: V.Vector Frame -> V.Vector Float
framesToInterleaved frames = V.generate (2 * V.length frames) $ \i ->
    let Frame l r = V.unsafeIndex frames (i `quot` 2)
    in realToFrac (if even i then l else r)
