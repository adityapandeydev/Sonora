{-# LANGUAGE ForeignFunctionInterface #-}
-- | Windows audio output through WASAPI, bound directly with the FFI.
--
-- Opens the default render endpoint in shared, event-driven mode with a
-- 32-bit float stereo stream; Windows converts to the device's own rate and
-- format. A dedicated OS thread owns every COM object: it waits for the
-- device's buffer event and fills free space with 'mixerRender'. No other
-- thread touches WASAPI, so control operations stay pure Haskell.
--
-- Only x86_64 is supported (one calling convention for Win32 and COM).
module Sonora.Audio.Wasapi
    ( WasapiOutput (..)
    , openWasapi
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Int (Int32, Int64)
import Data.IORef
import Data.Word (Word16, Word32, Word8)
import Foreign.C.String (peekCWString)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr
import Foreign.Storable
import Numeric (showHex)

import Sonora.Audio.Backend (AudioError (..))
import Sonora.Audio.Mixer (Mixer, mixerRender, mixerSampleRate)

data WasapiOutput = WasapiOutput
    { wasapiDeviceName    :: !String
    , wasapiBufferFrames  :: !Int
    , wasapiLost          :: IO Bool
    , wasapiUnderruns     :: IO Int
      -- | Stop the stream and release the device; waits for the output thread.
    , wasapiClose         :: IO ()
    }

type HRESULT = Int32
type Obj = Ptr ()

succeeded :: HRESULT -> Bool
succeeded hr = hr >= 0

-------------------------------------------------------------------------------
-- System functions

foreign import ccall safe "CoInitializeEx"
    c_CoInitializeEx :: Ptr () -> Word32 -> IO HRESULT
foreign import ccall safe "CoUninitialize"
    c_CoUninitialize :: IO ()
foreign import ccall safe "CoCreateInstance"
    c_CoCreateInstance :: Ptr Guid -> Ptr () -> Word32 -> Ptr Guid -> Ptr Obj -> IO HRESULT
foreign import ccall safe "PropVariantClear"
    c_PropVariantClear :: Ptr () -> IO HRESULT
foreign import ccall safe "CreateEventW"
    c_CreateEventW :: Ptr () -> Int32 -> Int32 -> Ptr () -> IO (Ptr ())
foreign import ccall safe "SetEvent"
    c_SetEvent :: Ptr () -> IO Int32
foreign import ccall safe "WaitForSingleObject"
    c_WaitForSingleObject :: Ptr () -> Word32 -> IO Word32
foreign import ccall safe "CloseHandle"
    c_CloseHandle :: Ptr () -> IO Int32

-------------------------------------------------------------------------------
-- COM vtable calls

type FnRelease = Obj -> IO Word32
type FnVoid = Obj -> IO HRESULT
type FnOutU32 = Obj -> Ptr Word32 -> IO HRESULT
type FnOutI64 = Obj -> Ptr Int64 -> IO HRESULT
type FnGetDefaultEndpoint = Obj -> Int32 -> Int32 -> Ptr Obj -> IO HRESULT
type FnActivate = Obj -> Ptr Guid -> Word32 -> Ptr () -> Ptr Obj -> IO HRESULT
type FnOpenPropertyStore = Obj -> Word32 -> Ptr Obj -> IO HRESULT
type FnGetValue = Obj -> Ptr () -> Ptr () -> IO HRESULT
type FnInitialize = Obj -> Int32 -> Word32 -> Int64 -> Int64 -> Ptr () -> Ptr Guid -> IO HRESULT
type FnSetEventHandle = Obj -> Ptr () -> IO HRESULT
type FnGetService = Obj -> Ptr Guid -> Ptr Obj -> IO HRESULT
type FnGetBuffer = Obj -> Word32 -> Ptr (Ptr Float) -> IO HRESULT
type FnReleaseBuffer = Obj -> Word32 -> Word32 -> IO HRESULT

foreign import ccall "dynamic" mkRelease :: FunPtr FnRelease -> FnRelease
foreign import ccall "dynamic" mkVoid :: FunPtr FnVoid -> FnVoid
foreign import ccall "dynamic" mkOutU32 :: FunPtr FnOutU32 -> FnOutU32
foreign import ccall "dynamic" mkOutI64 :: FunPtr FnOutI64 -> FnOutI64
foreign import ccall "dynamic" mkGetDefaultEndpoint :: FunPtr FnGetDefaultEndpoint -> FnGetDefaultEndpoint
foreign import ccall "dynamic" mkActivate :: FunPtr FnActivate -> FnActivate
foreign import ccall "dynamic" mkOpenPropertyStore :: FunPtr FnOpenPropertyStore -> FnOpenPropertyStore
foreign import ccall "dynamic" mkGetValue :: FunPtr FnGetValue -> FnGetValue
foreign import ccall "dynamic" mkInitialize :: FunPtr FnInitialize -> FnInitialize
foreign import ccall "dynamic" mkSetEventHandle :: FunPtr FnSetEventHandle -> FnSetEventHandle
foreign import ccall "dynamic" mkGetService :: FunPtr FnGetService -> FnGetService
foreign import ccall "dynamic" mkGetBuffer :: FunPtr FnGetBuffer -> FnGetBuffer
foreign import ccall "dynamic" mkReleaseBuffer :: FunPtr FnReleaseBuffer -> FnReleaseBuffer

-- | Look up method @index@ in a COM object's vtable.
method :: (FunPtr f -> f) -> Int -> Obj -> IO f
method mk index obj = do
    vtbl <- peek (castPtr obj) :: IO (Ptr (FunPtr ()))
    fp <- peekElemOff vtbl index
    return (mk (castFunPtr fp))

release :: Obj -> IO ()
release obj = unless (obj == nullPtr) $ do
    f <- method mkRelease 2 obj
    void (f obj)

-- Vtable indices (IUnknown occupies 0-2).
idxGetDefaultAudioEndpoint, idxActivate, idxOpenPropertyStore, idxGetValue :: Int
idxGetDefaultAudioEndpoint = 4  -- IMMDeviceEnumerator
idxActivate = 3                 -- IMMDevice
idxOpenPropertyStore = 4        -- IMMDevice
idxGetValue = 5                 -- IPropertyStore

idxInitialize, idxGetBufferSize, idxGetStreamLatency, idxGetCurrentPadding,
    idxStart, idxStop, idxSetEventHandle, idxGetService :: Int
idxInitialize = 3               -- IAudioClient
idxGetBufferSize = 4
idxGetStreamLatency = 5
idxGetCurrentPadding = 6
idxStart = 10
idxStop = 11
idxSetEventHandle = 13
idxGetService = 14

idxGetBuffer, idxReleaseBuffer :: Int
idxGetBuffer = 3                -- IAudioRenderClient
idxReleaseBuffer = 4

-------------------------------------------------------------------------------
-- GUIDs and constants

data Guid = Guid !Word32 !Word16 !Word16 ![Word8]

withGuid :: Guid -> (Ptr Guid -> IO a) -> IO a
withGuid (Guid d1 d2 d3 d4) act = allocaBytes 16 $ \p -> do
    pokeByteOff p 0 d1
    pokeByteOff p 4 d2
    pokeByteOff p 6 d3
    mapM_ (\(i, b) -> pokeByteOff p (8 + i) b) (zip [0 ..] (take 8 d4))
    act (castPtr p)

clsidMMDeviceEnumerator, iidIMMDeviceEnumerator, iidIAudioClient, iidIAudioRenderClient, pkeyFriendlyNameFmt :: Guid
clsidMMDeviceEnumerator = Guid 0xBCDE0395 0xE52F 0x467C [0x8E, 0x3D, 0xC4, 0x57, 0x92, 0x91, 0x69, 0x2E]
iidIMMDeviceEnumerator  = Guid 0xA95664D2 0x9614 0x4F35 [0xA7, 0x46, 0xDE, 0x8D, 0xB6, 0x36, 0x17, 0xE6]
iidIAudioClient         = Guid 0x1CB9AD4C 0xDBFA 0x4C32 [0xB1, 0x78, 0xC2, 0xF5, 0x68, 0xA7, 0x03, 0xB2]
iidIAudioRenderClient   = Guid 0xF294ACFC 0x3146 0x4483 [0xA7, 0xBF, 0xAD, 0xDC, 0xA7, 0xC2, 0x60, 0xE2]
pkeyFriendlyNameFmt     = Guid 0xA45C254E 0xDF1C 0x4EFD [0x80, 0x20, 0x67, 0xD1, 0x46, 0xA8, 0x50, 0xE0]

clsctxAll, coinitMultithreaded :: Word32
clsctxAll = 0x17
coinitMultithreaded = 0

eRender, eConsole, shareModeShared :: Int32
eRender = 0
eConsole = 0
shareModeShared = 0

streamFlags :: Word32
streamFlags = 0x00040000      -- AUDCLNT_STREAMFLAGS_EVENTCALLBACK
          .|. 0x80000000      -- AUDCLNT_STREAMFLAGS_AUTOCONVERTPCM
          .|. 0x08000000      -- AUDCLNT_STREAMFLAGS_SRC_DEFAULT_QUALITY

bufferFlagSilent :: Word32
bufferFlagSilent = 2

waitObject0 :: Word32
waitObject0 = 0

-------------------------------------------------------------------------------

hrError :: String -> HRESULT -> AudioError
hrError what hr = AudioError (fromIntegral hr) $
    what ++ " failed (HRESULT 0x" ++ showHex (fromIntegral hr :: Word32) "" ++ ")" ++ hint
  where
    hint = case fromIntegral hr :: Word32 of
        0x80070490 -> ": no audio output device is available"
        0x88890004 -> ": the audio device was removed or reset"
        0x8889000A -> ": the audio device is in use exclusively by another application"
        0x88890008 -> ": the stream format is not supported"
        _          -> ""

-- | Open the default output device and start feeding it from the mixer.
-- @bufferMillis@ is how much audio is queued ahead; larger values tolerate
-- longer pauses of the Haskell runtime at the cost of control latency.
openWasapi :: Int -> Mixer -> IO (Either AudioError WasapiOutput)
openWasapi bufferMillis mixer = do
    ready <- newEmptyMVar
    done <- newEmptyMVar
    stopping <- newIORef False
    lost <- newIORef False
    underruns <- newIORef (0 :: Int)
    eventVar <- newIORef nullPtr
    _ <- forkOS $
        (outputThread bufferMillis mixer ready stopping lost underruns eventVar
            `catch` \e -> do
                writeIORef lost True
                void (tryPutMVar ready (Left (AudioError (-1) (displayException (e :: SomeException))))))
        `finally` putMVar done ()
    result <- takeMVar ready
    return $ case result of
        Left err -> Left err
        Right (name, frames) -> Right WasapiOutput
            { wasapiDeviceName   = name
            , wasapiBufferFrames = frames
            , wasapiLost         = readIORef lost
            , wasapiUnderruns    = readIORef underruns
            , wasapiClose        = do
                writeIORef stopping True
                ev <- readIORef eventVar
                unless (ev == nullPtr) (void (c_SetEvent ev))
                readMVar done
            }

outputThread
    :: Int -> Mixer -> MVar (Either AudioError (String, Int)) -> IORef Bool -> IORef Bool
    -> IORef Int -> IORef (Ptr ()) -> IO ()
outputThread bufferMillis mixer ready stopping lost underruns eventVar = do
    cleanups <- newIORef [] :: IO (IORef [IO ()])
    let onCleanup act = modifyIORef cleanups (act :)
        runCleanups = readIORef cleanups >>= sequence_
        failWith err = putMVar ready (Left err)
        -- Run a setup step; Nothing means it failed and was reported.
        step what act = do
            hr <- act
            if succeeded hr then return True else failWith (hrError what hr) >> return False
        acquire what act = alloca $ \out -> do
            poke out nullPtr
            ok <- step what (act out)
            if ok
                then do
                    obj <- peek out
                    onCleanup (release obj)
                    return (Just obj)
                else return Nothing
    flip finally runCleanups $ do
        okInit <- step "CoInitializeEx" (c_CoInitializeEx nullPtr coinitMultithreaded)
        when okInit $ do
            onCleanup c_CoUninitialize
            mEnum <- acquire "Creating the device enumerator" $ \out ->
                withGuid clsidMMDeviceEnumerator $ \clsid -> withGuid iidIMMDeviceEnumerator $ \iid ->
                    c_CoCreateInstance clsid nullPtr clsctxAll iid out
            mDevice <- maybe (return Nothing) (\enum -> acquire "Finding the default output device" $ \out -> do
                f <- method mkGetDefaultEndpoint idxGetDefaultAudioEndpoint enum
                f enum eRender eConsole out) mEnum
            case mDevice of
                Nothing -> return ()
                Just device -> do
                    name <- deviceName device
                    mClient <- acquire "Activating the audio client" $ \out -> do
                        f <- method mkActivate idxActivate device
                        withGuid iidIAudioClient $ \iid -> f device iid clsctxAll nullPtr out
                    mapM_ (runClient name) mClient
  where
    runClient name client = do
        let sampleRate = mixerSampleRate mixer
            duration = fromIntegral (max 10 bufferMillis) * 10000 :: Int64  -- 100 ns units
        hrInit <- allocaBytes 18 $ \fmt -> do
            pokeByteOff fmt 0 (3 :: Word16)                            -- WAVE_FORMAT_IEEE_FLOAT
            pokeByteOff fmt 2 (2 :: Word16)                            -- channels
            pokeByteOff fmt 4 (fromIntegral sampleRate :: Word32)
            pokeByteOff fmt 8 (fromIntegral sampleRate * 8 :: Word32)  -- bytes per second
            pokeByteOff fmt 12 (8 :: Word16)                           -- block align
            pokeByteOff fmt 14 (32 :: Word16)                          -- bits per sample
            pokeByteOff fmt 16 (0 :: Word16)                           -- cbSize
            f <- method mkInitialize idxInitialize client
            f client shareModeShared streamFlags duration 0 fmt nullPtr
        if not (succeeded hrInit)
            then putMVar ready (Left (hrError "Initializing the audio stream" hrInit))
            else do
                event <- c_CreateEventW nullPtr 0 0 nullPtr
                if event == nullPtr
                    then putMVar ready (Left (AudioError (-1) "CreateEventW failed"))
                    else do
                        writeIORef eventVar event
                        flip finally (writeIORef eventVar nullPtr >> void (c_CloseHandle event)) $
                            runStream name client event

    runStream name client event = do
        setEvent <- method mkSetEventHandle idxSetEventHandle client
        hrEv <- setEvent client event
        bufferFrames <- alloca $ \p -> do
            f <- method mkOutU32 idxGetBufferSize client
            hr <- f client p
            if succeeded hr then Just . fromIntegral <$> peek p else return Nothing
        mRender <- alloca $ \out -> do
            poke out nullPtr
            f <- method mkGetService idxGetService client
            hr <- withGuid iidIAudioRenderClient $ \iid -> f client iid out
            if succeeded hr then Just <$> peek out else return Nothing
        case (succeeded hrEv, bufferFrames, mRender) of
            (True, Just frames, Just render) -> flip finally (release render) $ do
                getBuffer <- method mkGetBuffer idxGetBuffer render
                releaseBuffer <- method mkReleaseBuffer idxReleaseBuffer render
                getPadding <- method mkOutU32 idxGetCurrentPadding client
                start <- method mkVoid idxStart client
                stop <- method mkVoid idxStop client
                -- Pre-fill with silence so the first period does not glitch.
                prefill <- alloca $ \pp -> do
                    hr <- getBuffer render (fromIntegral frames) pp
                    if succeeded hr then releaseBuffer render (fromIntegral frames) bufferFlagSilent else return hr
                hrStart <- if succeeded prefill then start client else return prefill
                if not (succeeded hrStart)
                    then putMVar ready (Left (hrError "Starting the audio stream" hrStart))
                    else do
                        latency <- streamLatency client
                        putMVar ready (Right (name, max frames latency))
                        feed frames getBuffer releaseBuffer getPadding render client event
                        void (stop client)
            _ -> putMVar ready (Left (AudioError (-1) "Configuring the audio stream failed"))

    streamLatency client = alloca $ \p -> do
        f <- method mkOutI64 idxGetStreamLatency client
        hr <- f client p
        if succeeded hr
            then do
                hns <- peek p
                return (fromIntegral ((hns * fromIntegral (mixerSampleRate mixer)) `div` 10000000))
            else return 0

    feed frames getBuffer releaseBuffer getPadding render client event =
        alloca $ \padPtr -> alloca $ \bufPtr -> do
            let loop = do
                    stop <- readIORef stopping
                    unless stop $ do
                        w <- c_WaitForSingleObject event 200
                        hrPad <- getPadding client padPtr
                        if not (succeeded hrPad)
                            then writeIORef lost True
                            else do
                                padding <- fromIntegral <$> peek padPtr
                                when (w == waitObject0 && padding == 0) $
                                    modifyIORef' underruns (+ 1)
                                let avail = frames - padding
                                ok <- if avail <= 0 then return True else do
                                    hrBuf <- getBuffer render (fromIntegral avail) bufPtr
                                    if not (succeeded hrBuf) then return False else do
                                        out <- peek bufPtr
                                        mixerRender mixer avail out
                                            `onException` fillBytes out 0 (avail * 8)
                                        succeeded <$> releaseBuffer render (fromIntegral avail) 0
                                if ok then loop else writeIORef lost True
            loop

-- | Friendly name of an endpoint, or a generic label if unavailable.
deviceName :: Obj -> IO String
deviceName device = handle (\e -> const (return fallback) (e :: SomeException)) $
    alloca $ \storeOut -> do
        poke storeOut nullPtr
        open <- method mkOpenPropertyStore idxOpenPropertyStore device
        hr <- open device 0 storeOut  -- STGM_READ
        if not (succeeded hr) then return fallback else do
            store <- peek storeOut
            flip finally (release store) $
                allocaBytes 20 $ \key -> allocaBytes 24 $ \pv -> do
                    withGuid pkeyFriendlyNameFmt $ \g -> copyGuid g key
                    pokeByteOff key 16 (14 :: Word32)  -- PKEY_Device_FriendlyName pid
                    fillBytes pv 0 24
                    getValue <- method mkGetValue idxGetValue store
                    hr2 <- getValue store key pv
                    if not (succeeded hr2) then return fallback else do
                        vt <- peekByteOff pv 0 :: IO Word16
                        name <- if vt == 31  -- VT_LPWSTR
                            then peekByteOff pv 8 >>= \s -> if s == nullPtr then return fallback else peekCWString s
                            else return fallback
                        void (c_PropVariantClear pv)
                        return name
  where
    fallback = "default output device"
    copyGuid src dst = mapM_ (\i -> peekByteOff src i >>= \b -> pokeByteOff dst i (b :: Word8)) [0 .. 15]
