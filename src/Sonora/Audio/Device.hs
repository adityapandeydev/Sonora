{-# LANGUAGE CPP #-}
-- | The system audio output device.
--
-- Playback, mixing, and transport are done by "Sonora.Audio.Mixer"; the
-- platform driver only hands finished sample blocks to the operating
-- system. On Windows the driver is WASAPI ("Sonora.Audio.Wasapi").
module Sonora.Audio.Device
    ( openDeviceBackend
    , withDeviceBackend
    , defaultBufferMillis
    ) where

import Control.Exception (bracket)

import Sonora.Audio.Backend
#if defined(mingw32_HOST_OS)
import Sonora.Audio.Mixer (mixerBackend, newMixer)
import Sonora.Audio.Wasapi
#endif

-- | Audio queued ahead of the speaker. Covers runtime pauses (such as
-- garbage collection) at the cost of this much delay for transport changes.
defaultBufferMillis :: Int
defaultBufferMillis = 100

-- | Open the default output device. Buffers passed to the backend are
-- interpreted at the given sample rate; the system converts to the device rate.
openDeviceBackend :: Int -> IO (Either AudioError AudioBackend)
openDeviceBackend sampleRate
    | sampleRate <= 0 = return (Left (AudioError (-2) "Sample rate must be positive"))
#if defined(mingw32_HOST_OS)
    | otherwise = do
        mixer <- newMixer sampleRate
        result <- openWasapi defaultBufferMillis mixer
        case result of
            Left err -> return (Left err)
            Right out -> Right <$> mixerBackend mixer
                ("WASAPI: " ++ wasapiDeviceName out)
                (wasapiBufferFrames out)
                (wasapiLost out)
                (wasapiUnderruns out)
                (wasapiClose out)
#else
    | otherwise = return (Left (AudioError (-1) "No audio output driver is available on this platform yet"))
#endif

-- | Open a device for the duration of an action, closing it afterwards even
-- if the action throws.
withDeviceBackend :: Int -> (AudioBackend -> IO a) -> IO (Either AudioError a)
withDeviceBackend sampleRate action =
    bracket (openDeviceBackend sampleRate) (either (const (return ())) backendClose) $
        either (return . Left) (fmap Right . action)
