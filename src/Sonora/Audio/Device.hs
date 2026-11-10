{-# LANGUAGE ForeignFunctionInterface #-}
-- | Native audio output through the bundled miniaudio shim (cbits/).
--
-- Opens the system default playback device as stereo float32. The device
-- runs its own realtime thread that reads buffers prepared here; Haskell code
-- is never called from that thread.
module Sonora.Audio.Device
    ( openDeviceBackend
    , withDeviceBackend
      -- * Testing
    , simulateNativeVoice
    ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Foreign.C.String (CString, peekCString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekByteOff, pokeElemOff)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Signal.Core (Frame (..))

data SonoraAudio

foreign import ccall safe "sonora_audio_open"
    c_open :: Word32 -> Ptr Int32 -> IO (Ptr SonoraAudio)
foreign import ccall safe "sonora_audio_close"
    c_close :: Ptr SonoraAudio -> IO ()
foreign import ccall unsafe "sonora_audio_error_string"
    c_errorString :: Int32 -> IO CString
foreign import ccall safe "sonora_audio_device_name"
    c_deviceName :: Ptr SonoraAudio -> CString -> Word32 -> IO ()
foreign import ccall unsafe "sonora_audio_alloc_frames"
    c_allocFrames :: Word64 -> IO (Ptr Float)
foreign import ccall unsafe "sonora_audio_free_frames"
    c_freeFrames :: Ptr Float -> IO ()
foreign import ccall safe "sonora_audio_load"
    c_load :: Ptr SonoraAudio -> Int32 -> Ptr Float -> Word64 -> IO Int32
foreign import ccall safe "sonora_audio_set_playing"
    c_setPlaying :: Ptr SonoraAudio -> Int32 -> Int32 -> IO ()
foreign import ccall safe "sonora_audio_seek"
    c_seek :: Ptr SonoraAudio -> Int32 -> Word64 -> IO ()
foreign import ccall safe "sonora_audio_set_loop"
    c_setLoop :: Ptr SonoraAudio -> Int32 -> Int32 -> Word64 -> Word64 -> IO ()
foreign import ccall safe "sonora_audio_set_gain"
    c_setGain :: Ptr SonoraAudio -> Int32 -> Float -> IO ()
foreign import ccall unsafe "sonora_audio_set_master_gain"
    c_setMasterGain :: Ptr SonoraAudio -> Float -> IO ()
foreign import ccall safe "sonora_audio_status"
    c_status :: Ptr SonoraAudio -> Int32 -> Ptr () -> IO ()
foreign import ccall unsafe "sonora_audio_device_lost"
    c_deviceLost :: Ptr SonoraAudio -> IO Int32
foreign import ccall safe "sonora_audio_simulate"
    c_simulate :: Word64 -> Word64 -> Int32 -> Int32 -> Word64 -> Word64
               -> Ptr Word32 -> Word32 -> Ptr () -> IO ()

-- | Open the default output device. Buffers passed to the backend are
-- interpreted at the given sample rate; miniaudio converts to the device rate.
openDeviceBackend :: Int -> IO (Either AudioError AudioBackend)
openDeviceBackend sampleRate
    | sampleRate <= 0 = return (Left (AudioError (-2) "Sample rate must be positive"))
    | otherwise = do
        (handle, code) <- alloca $ \errPtr -> do
            h <- c_open (fromIntegral sampleRate) errPtr
            c <- peek errPtr
            return (h, c)
        if handle == nullPtr
            then do
                msg <- c_errorString code >>= peekCString
                return (Left (AudioError (fromIntegral code) msg))
            else do
                name <- allocaBytes 256 $ \buf -> c_deviceName handle buf 256 >> peekCString buf
                ref <- newMVar (Just handle)
                return (Right (mkBackend sampleRate name ref))

-- | Open a device for the duration of an action, closing it afterwards even
-- if the action throws.
withDeviceBackend :: Int -> (AudioBackend -> IO a) -> IO (Either AudioError a)
withDeviceBackend sampleRate action =
    bracket (openDeviceBackend sampleRate) (either (const (return ())) backendClose) $
        either (return . Left) (fmap Right . action)

mkBackend :: Int -> String -> MVar (Maybe (Ptr SonoraAudio)) -> AudioBackend
mkBackend sampleRate name ref = AudioBackend
    { backendDescription   = "miniaudio: " ++ name
    , backendSampleRate    = sampleRate
    , backendLoad          = load
    , backendSetPlaying    = \v p -> withOpen (\h -> c_setPlaying h (voiceIndex v) (fromBool p))
    , backendSeek          = \v pos -> withOpen (\h -> c_seek h (voiceIndex v) pos)
    , backendSetLoop       = \v l -> withOpen $ \h -> case l of
        Just (s, e) -> c_setLoop h (voiceIndex v) 1 s e
        Nothing     -> c_setLoop h (voiceIndex v) 0 0 0
    , backendSetGain       = \v g -> withOpen (\h -> c_setGain h (voiceIndex v) (realToFrac g))
    , backendSetMasterGain = \g -> withOpen (\h -> c_setMasterGain h (realToFrac g))
    , backendStatus        = status
    , backendDeviceLost    = withMVar ref $ maybe (return False) (fmap (/= 0) . c_deviceLost)
    , backendClose         = modifyMVar_ ref $ \mh -> mapM_ c_close mh >> return Nothing
    }
  where
    withOpen f = withMVar ref (mapM_ f)

    fromBool b = if b then 1 else 0

    load voice frames = do
        let n = V.length frames
        -- Convert outside the lock; the shim takes ownership of the buffer.
        buf <- if n == 0 then return nullPtr else c_allocFrames (fromIntegral n)
        if n > 0 && buf == nullPtr
            then ioError (userError "Sonora audio: out of memory allocating playback buffer")
            else do
                forM_ [0 .. n - 1] $ \i -> do
                    let Frame l r = V.unsafeIndex frames i
                    pokeElemOff buf (2 * i) (realToFrac l :: Float)
                    pokeElemOff buf (2 * i + 1) (realToFrac r :: Float)
                withMVar ref $ \mh -> case mh of
                    Just h  -> () <$ c_load h (voiceIndex voice) buf (fromIntegral n)
                    Nothing -> () <$ c_freeFrames buf

    status voice = withMVar ref $ \mh -> case mh of
        Nothing -> return (VoiceStatus 0 0 0 False)
        Just h  -> allocaBytes 24 $ \p -> do
            c_status h (voiceIndex voice) p
            pos <- peekByteOff p 0
            len <- peekByteOff p 8
            ends <- peekByteOff p 16
            playing <- peekByteOff p 20 :: IO Int32
            return (VoiceStatus pos len ends (playing /= 0))

-- | Run the native realtime mixing routine over a silent buffer described by
-- the model, one device callback per chunk size. No device is opened.
simulateNativeVoice :: VoiceModel -> [Word32] -> IO VoiceStatus
simulateNativeVoice vm chunks =
    withArrayLen chunks $ \n chunkPtr -> allocaBytes 24 $ \p -> do
        let (loopOn, ls, le) = maybe (0, 0, 0) (\(s, e) -> (1, s, e)) (vmLoop vm)
        c_simulate (vmLength vm) (vmPosition vm) (if vmPlaying vm then 1 else 0)
                   loopOn ls le chunkPtr (fromIntegral n) p
        VoiceStatus <$> peekByteOff p 0 <*> peekByteOff p 8 <*> peekByteOff p 16
                    <*> ((/= (0 :: Int32)) <$> peekByteOff p 20)
