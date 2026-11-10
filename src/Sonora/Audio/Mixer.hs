{-# LANGUAGE BangPatterns #-}
-- | The realtime playback mixer.
--
-- A 'Mixer' holds one buffer per 'Voice' and renders blocks of interleaved
-- stereo float samples on demand. A device driver calls 'mixerRender' from
-- its output thread; control operations run on any other thread.
--
-- Concurrency: each voice is an 'IORef' updated with atomic modifications,
-- so control operations never wait for a render and a render never waits
-- for control. A render works on a snapshot and commits its advance only if
-- no load or seek happened meanwhile (tracked by an epoch counter); a
-- concurrent seek therefore always wins. Play/pause, loop, and gain changes
-- are merged with the render's advance instead of discarding it.
module Sonora.Audio.Mixer
    ( Mixer
    , newMixer
    , mixerSampleRate
    , mixerRender
    , mixerBackend
      -- * Inspection (tests)
    , mixerFrames
    , mixerTargetGain
    , mixerMasterGain
    ) where

import Control.Monad (unless, when)
import Data.IORef
import Data.Word (Word64)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Backend
import Sonora.Signal.Core (Frame (..))

data VoiceState = VoiceState
    { vsModel  :: !VoiceModel
    , vsFrames :: !(V.Vector Frame)
    , vsGain   :: !Double   -- ^ Target gain.
    , vsEpoch  :: !Word64   -- ^ Bumped by load and seek.
    }

data MixerVoice = MixerVoice
    { mvState       :: !(IORef VoiceState)
    , mvCurrentGain :: !(IORef Double)  -- ^ Smoothed gain; owned by the render thread.
    }

data Mixer = Mixer
    { mixerSampleRate   :: !Int
    , mixSmoothing      :: !Double
    , mixSong           :: !MixerVoice
    , mixAudition       :: !MixerVoice
    , mixMasterTarget   :: !(IORef Double)
    , mixMasterCurrent  :: !(IORef Double)
    }

-- | Gain changes are smoothed with a one-pole filter of about this time constant.
gainSmoothingSeconds :: Double
gainSmoothingSeconds = 0.005

newMixer :: Int -> IO Mixer
newMixer sampleRate = do
    let voice = do
            st <- newIORef (VoiceState emptyVoiceModel V.empty 1 0)
            g <- newIORef 1
            return (MixerVoice st g)
    song <- voice
    aud <- voice
    mt <- newIORef 1
    mc <- newIORef 1
    let sr = fromIntegral (max 1 sampleRate) :: Double
    return Mixer
        { mixerSampleRate  = sampleRate
        , mixSmoothing     = 1 - exp (-1 / (gainSmoothingSeconds * sr))
        , mixSong          = song
        , mixAudition      = aud
        , mixMasterTarget  = mt
        , mixMasterCurrent = mc
        }

voiceOf :: Mixer -> Voice -> MixerVoice
voiceOf m SongVoice     = mixSong m
voiceOf m AuditionVoice = mixAudition m

modifyVoice :: Mixer -> Voice -> (VoiceState -> VoiceState) -> IO ()
modifyVoice m v f = atomicModifyIORef' (mvState (voiceOf m v)) (\s -> (f s, ()))

-- | Render @n@ frames into @out@ (2 * n floats, overwritten), advancing
-- every playing voice.
mixerRender :: Mixer -> Int -> Ptr Float -> IO ()
mixerRender m n out = when (n > 0) $ do
    fillBytes out 0 (n * 2 * 4)
    renderVoice (mixSmoothing m) (mixSong m) n out
    renderVoice (mixSmoothing m) (mixAudition m) n out
    target <- readIORef (mixMasterTarget m)
    g0 <- readIORef (mixMasterCurrent m)
    let k = mixSmoothing m
        clamp x = max (-1) (min 1 x)
        go !i !g
            | i >= n = return g
            | otherwise = do
                let !g' = g + (target - g) * k
                l <- peekElemOff out (2 * i)
                r <- peekElemOff out (2 * i + 1)
                pokeElemOff out (2 * i) (realToFrac (clamp (realToFrac l * g')))
                pokeElemOff out (2 * i + 1) (realToFrac (clamp (realToFrac r * g')))
                go (i + 1) g'
    g1 <- go 0 g0
    writeIORef (mixMasterCurrent m) g1

renderVoice :: Double -> MixerVoice -> Int -> Ptr Float -> IO ()
renderVoice k mv n out = do
    snap <- readIORef (mvState mv)
    let vm = vsModel snap
    when (vmPlaying vm) $ do
        let frames = vsFrames snap
            len = vmLength vm
            target = vsGain snap
            loopEnd = maybe maxBound snd (activeLoop vm)
            loopStart = maybe 0 fst (activeLoop vm)
            -- Mirrors advanceVoice frame by frame: wrap only when crossing
            -- the loop end, stop at the end of the buffer.
            go !i !pos !g
                | i >= n || pos >= len = return g
                | otherwise = do
                    let Frame l r = V.unsafeIndex frames (fromIntegral pos)
                        !g' = g + (target - g) * k
                        !next = let p = pos + 1 in if p == loopEnd then loopStart else p
                    ol <- peekElemOff out (2 * i)
                    or' <- peekElemOff out (2 * i + 1)
                    pokeElemOff out (2 * i) (ol + realToFrac (l * g'))
                    pokeElemOff out (2 * i + 1) (or' + realToFrac (r * g'))
                    go (i + 1) next g'
        g0 <- readIORef (mvCurrentGain mv)
        g1 <- go 0 (vmPosition vm) g0
        writeIORef (mvCurrentGain mv) g1
        let advanced = advanceVoice (fromIntegral n) vm
        atomicModifyIORef' (mvState mv) $ \cur ->
            if vsEpoch cur /= vsEpoch snap
                then (cur, ())
                else let cm = vsModel cur
                     in ( cur { vsModel = cm { vmPosition = vmPosition advanced
                                             , vmPlaying  = vmPlaying cm && vmPlaying advanced
                                             , vmEndCount = vmEndCount advanced
                                             } }
                        , () )

-- | Wrap a mixer as an 'AudioBackend'. The driver supplies its description,
-- latency, loss and underrun reporting, and shutdown. After 'backendClose'
-- every operation is a no-op and status reports an empty voice.
mixerBackend :: Mixer -> String -> Int -> IO Bool -> IO Int -> IO () -> IO AudioBackend
mixerBackend m description latency lost underruns closeDriver = do
    closed <- newIORef False
    let whenOpen act = readIORef closed >>= \c -> unless c act
        onVoice v f = whenOpen (modifyVoice m v f)
        onModel v f = onVoice v (\s -> s { vsModel = f (vsModel s) })
        bump s = s { vsEpoch = vsEpoch s + 1 }
    return AudioBackend
        { backendDescription   = description
        , backendSampleRate    = mixerSampleRate m
        , backendLoad          = \v frames -> do
            onVoice v $ \s -> bump s
                { vsFrames = frames
                , vsModel  = loadVoiceModel (fromIntegral (V.length frames)) (vsModel s)
                }
            -- A fresh buffer starts at its target gain rather than ramping.
            whenOpen $ do
                s <- readIORef (mvState (voiceOf m v))
                writeIORef (mvCurrentGain (voiceOf m v)) (vsGain s)
        , backendSetPlaying    = \v p -> onModel v (setPlayingModel p)
        , backendSeek          = \v pos -> onVoice v (\s -> bump s { vsModel = seekModel pos (vsModel s) })
        , backendSetLoop       = \v l -> onModel v (setLoopModel l)
        , backendSetGain       = \v g -> onVoice v (\s -> s { vsGain = max 0 g })
        , backendSetMasterGain = \g -> whenOpen (writeIORef (mixMasterTarget m) (max 0 g))
        , backendStatus        = \v -> do
            c <- readIORef closed
            if c then return (voiceModelStatus emptyVoiceModel)
                 else voiceModelStatus . vsModel <$> readIORef (mvState (voiceOf m v))
        , backendDeviceLost    = lost
        , backendLatencyFrames = latency
        , backendUnderruns     = underruns
        , backendClose         = do
            wasClosed <- atomicModifyIORef' closed (\c -> (True, c))
            unless wasClosed closeDriver
        }

mixerFrames :: Mixer -> Voice -> IO (V.Vector Frame)
mixerFrames m v = vsFrames <$> readIORef (mvState (voiceOf m v))

mixerTargetGain :: Mixer -> Voice -> IO Double
mixerTargetGain m v = vsGain <$> readIORef (mvState (voiceOf m v))

mixerMasterGain :: Mixer -> IO Double
mixerMasterGain m = readIORef (mixMasterTarget m)
