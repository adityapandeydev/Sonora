{-# LANGUAGE BangPatterns #-}
-- | The realtime playback mixer.
--
-- A 'Mixer' holds one buffer per 'Voice' and renders blocks of interleaved
-- stereo float samples on demand. A device driver calls 'mixerRender' from
-- its output thread; control operations run on any other thread.
--
-- Concurrency: the render thread is the only writer of the mixer state.
-- Control operations are queued as pure updates and applied, in order, at
-- the start of the next block. Reads apply the queued updates to the last
-- published state, so an operation is visible as soon as it returns. Neither
-- side ever waits for the other.
--
-- Clicks: every cut in a playing stream (pause, seek, buffer change, loop
-- wrap) lets the old stream continue briefly as a fading /tail/ while the new
-- one fades in. Positions are unaffected and follow 'advanceVoice' exactly.
--
-- Clock: each voice remembers which buffer position went to which output
-- frame. Given how many output frames the device has played, 'backendHeard'
-- reports the position being heard, correct across seeks and loops.
module Sonora.Audio.Mixer
    ( Mixer
    , newMixer
    , newMixerWith
    , defaultFadeFrames
    , mixerSampleRate
    , mixerRender
    , mixerRenderedFrames
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

-- | The end of a stream that was cut, fading out.
data Tail = Tail
    { tailFrames :: !(V.Vector Frame)
    , tailPos    :: !Int
    , tailLeft   :: !Int     -- ^ Frames of fade remaining.
    , tailLevel  :: !Double  -- ^ Level at the cut (below 1 if still fading in).
    }

-- | From output frame 'segOut' on, the voice played from 'segPos'.
data Segment = Segment
    { segOut     :: !Word64
    , segPos     :: !Word64
    , segPlaying :: !Bool
    , segGen     :: !Word64
    }

data VoiceState = VoiceState
    { vsModel    :: !VoiceModel
    , vsFrames   :: !(V.Vector Frame)
    , vsGain     :: !Double     -- ^ Target gain.
    , vsCurrent  :: !Double     -- ^ Smoothed gain.
    , vsFadeIn   :: !Int        -- ^ Frames of fade-in remaining.
    , vsTails    :: ![Tail]
    , vsGen      :: !Word64     -- ^ Bumped by load and seek.
    , vsGenStart :: !Word64     -- ^ Position set by that load or seek.
    , vsHistory  :: ![Segment]  -- ^ Newest first.
    }

data MixerState = MixerState
    { msSong          :: !VoiceState
    , msAudition      :: !VoiceState
    , msMasterTarget  :: !Double
    , msMasterCurrent :: !Double
    , msRendered      :: !Word64  -- ^ Output frames rendered so far.
    }

-- | Last published state and the updates queued since, newest first.
data Shared = Shared !MixerState ![MixerState -> MixerState]

data Mixer = Mixer
    { mixerSampleRate :: !Int
    , mixSmoothing    :: !Double
    , mixFade         :: !Int
    , mixShared       :: !(IORef Shared)
    }

-- | Gain changes are smoothed with a one-pole filter of about this time constant.
gainSmoothingSeconds :: Double
gainSmoothingSeconds = 0.005

-- | Length of the fades that hide cuts: 4 ms.
defaultFadeFrames :: Int -> Int
defaultFadeFrames sampleRate = sampleRate * 4 `div` 1000

maxTails :: Int
maxTails = 4

newMixer :: Int -> IO Mixer
newMixer sampleRate = newMixerWith sampleRate (defaultFadeFrames sampleRate)

-- | A mixer with a given fade length in frames (0 disables fades).
newMixerWith :: Int -> Int -> IO Mixer
newMixerWith sampleRate fade = do
    ref <- newIORef (Shared emptyState [])
    let sr = fromIntegral (max 1 sampleRate) :: Double
    return Mixer
        { mixerSampleRate = sampleRate
        , mixSmoothing    = 1 - exp (-1 / (gainSmoothingSeconds * sr))
        , mixFade         = max 0 fade
        , mixShared       = ref
        }

emptyState :: MixerState
emptyState = MixerState emptyVoice emptyVoice 1 1 0

emptyVoice :: VoiceState
emptyVoice = VoiceState emptyVoiceModel V.empty 1 1 0 [] 0 0 []

voiceState :: Voice -> MixerState -> VoiceState
voiceState SongVoice     = msSong
voiceState AuditionVoice = msAudition

onVoice :: Voice -> (VoiceState -> VoiceState) -> MixerState -> MixerState
onVoice SongVoice f s     = s { msSong = f (msSong s) }
onVoice AuditionVoice f s = s { msAudition = f (msAudition s) }

applyQueued :: Shared -> MixerState
applyQueued (Shared st queued) = foldr ($) st queued

currentState :: Mixer -> IO MixerState
currentState m = applyQueued <$> readIORef (mixShared m)

enqueue :: Mixer -> (MixerState -> MixerState) -> IO ()
enqueue m f = atomicModifyIORef' (mixShared m) (\(Shared st q) -> (Shared st (f : q), ()))

-------------------------------------------------------------------------------
-- Control updates

-- | Let a playing stream fade out from where it is.
cut :: Int -> VoiceState -> VoiceState
cut fade v
    | fade <= 0 || not (vmPlaying vm) || vmPosition vm >= vmLength vm = v
    | otherwise = v { vsTails = take maxTails (Tail (vsFrames v) (fromIntegral (vmPosition vm)) fade level : vsTails v) }
  where
    vm = vsModel v
    level = fadeInLevel fade (vsFadeIn v)

-- | Level of a stream with the given frames of fade-in remaining.
fadeInLevel :: Int -> Int -> Double
fadeInLevel fade left
    | left <= 0 || fade <= 0 = 1
    | otherwise = fromIntegral (fade - left) / fromIntegral fade

-- | Fade in from the current position. Position 0 is the natural start of
-- the buffer and needs no fade.
fadeIn :: Int -> VoiceState -> VoiceState
fadeIn fade v = v { vsFadeIn = if vmPosition (vsModel v) == 0 then 0 else fade }

loadUpdate, replaceUpdate :: Int -> V.Vector Frame -> VoiceState -> VoiceState
loadUpdate fade frames v = (cut fade v)
    { vsFrames   = frames
    , vsModel    = loadVoiceModel (fromIntegral (V.length frames)) (vsModel v)
    , vsCurrent  = vsGain v  -- a fresh buffer starts at its target gain
    , vsFadeIn   = 0
    , vsGen      = vsGen v + 1
    , vsGenStart = 0
    }
replaceUpdate fade frames v = fadeIn fade (cut fade v)
    { vsFrames = frames
    , vsModel  = replaceVoiceModel (fromIntegral (V.length frames)) (vsModel v)
    }

playingUpdate :: Int -> Bool -> VoiceState -> VoiceState
playingUpdate fade p v
    | vmPlaying vm == vmPlaying vm' = v'
    | vmPlaying vm' = fadeIn fade v'
    | otherwise = (cut fade v) { vsModel = vm' }
  where
    vm = vsModel v
    vm' = setPlayingModel p vm
    v' = v { vsModel = vm' }

seekUpdate :: Int -> Word64 -> VoiceState -> VoiceState
seekUpdate fade pos v =
    let vm' = seekModel pos (vsModel v)
        v' = (cut fade v) { vsModel = vm', vsGen = vsGen v + 1, vsGenStart = vmPosition vm' }
    in fadeIn fade v'

-------------------------------------------------------------------------------
-- Rendering

-- | Render @n@ frames into @out@ (2 * n floats, overwritten), advancing
-- every playing voice. Must only be called from one thread.
mixerRender :: Mixer -> Int -> Ptr Float -> IO ()
mixerRender m n out = when (n > 0) $ do
    shared@(Shared _ taken) <- readIORef (mixShared m)
    let st = applyQueued shared
        k = mixSmoothing m
        start = msRendered st
        keep = fromIntegral (mixerSampleRate m)
        keepFrom = if start > keep then start - keep else 0
    fillBytes out 0 (n * 2 * 4)
    song <- renderVoice m start n out (msSong st)
    aud <- renderVoice m start n out (msAudition st)
    let clamp x = max (-1) (min 1 x)
        target = msMasterTarget st
        go !i !g
            | i >= n = return g
            | otherwise = do
                let !g' = g + (target - g) * k
                l <- peekElemOff out (2 * i)
                r <- peekElemOff out (2 * i + 1)
                pokeElemOff out (2 * i) (realToFrac (clamp (realToFrac l * g')))
                pokeElemOff out (2 * i + 1) (realToFrac (clamp (realToFrac r * g')))
                go (i + 1) g'
    master <- go 0 (msMasterCurrent st)
    let !song' = prune keepFrom song
        !aud' = prune keepFrom aud
        !st' = st { msSong = song', msAudition = aud'
                  , msMasterCurrent = master, msRendered = start + fromIntegral n }
        !applied = length taken
    -- Keep updates queued while this block was rendering.
    atomicModifyIORef' (mixShared m) $ \(Shared _ q) ->
        let !rest = forceSpine (take (length q - applied) q) in (Shared st' rest, ())

-- | Evaluate a list's spine so it does not retain what it was taken from.
forceSpine :: [a] -> [a]
forceSpine xs = length xs `seq` xs

-- | Render one voice over a block, split into runs at loop wraps and at the
-- end of the buffer.
renderVoice :: Mixer -> Word64 -> Int -> Ptr Float -> VoiceState -> IO VoiceState
renderVoice m start n out = go 0
  where
    fade = mixFade m
    go !off v0
        | off >= n = return v0
        | not (vmPlaying vm) = renderTails fade off (n - off) out v
        | boundary <= pos = go off (finish v)  -- nothing left to play
        | otherwise = do
            let run = fromIntegral (min (fromIntegral (n - off)) (boundary - pos)) :: Int
                pos' = pos + fromIntegral run
            v1 <- renderTails fade off run out v
            v2 <- renderMain (mixSmoothing m) fade off run out v1
            let v3 = v2 { vsModel = vm { vmPosition = pos' } }
            go (off + run) $ if pos' < boundary then v3 else case looping of
                Just (s, _) -> fadeIn fade ((cut fade v3) { vsModel = vm { vmPosition = s } })
                Nothing     -> finish v3
      where
        v = record (start + fromIntegral off) v0
        vm = vsModel v
        pos = vmPosition vm
        looping = case activeLoop vm of
            Just (s, e) | pos < e -> Just (s, e)
            _                     -> Nothing
        boundary = maybe (vmLength vm) snd looping
    finish v =
        let vm = vsModel v
        in v { vsModel = vm { vmPosition = vmLength vm, vmPlaying = False, vmEndCount = vmEndCount vm + 1 } }

renderMain :: Double -> Int -> Int -> Int -> Ptr Float -> VoiceState -> IO VoiceState
renderMain k fade off run out v = do
    let frames = vsFrames v
        base = fromIntegral (vmPosition (vsModel v)) :: Int
        target = vsGain v
        go !i !g !left
            | i >= run = return (g, left)
            | otherwise = do
                let Frame l r = V.unsafeIndex frames (base + i)
                    !g' = g + (target - g) * k
                    !a = if left <= 0 then g' else g' * fadeInLevel fade left
                addFrame out (off + i) (l * a) (r * a)
                go (i + 1) g' (max 0 (left - 1))
    (g, left) <- go 0 (vsCurrent v) (vsFadeIn v)
    return v { vsCurrent = g, vsFadeIn = left }

renderTails :: Int -> Int -> Int -> Ptr Float -> VoiceState -> IO VoiceState
renderTails fade off run out v
    | null (vsTails v) = return v
    | otherwise = do
        let gain = vsCurrent v
            one t = do
                let count = minimum [run, tailLeft t, V.length (tailFrames t) - tailPos t]
                    go !i
                        | i >= count = return ()
                        | otherwise = do
                            let Frame l r = V.unsafeIndex (tailFrames t) (tailPos t + i)
                                !a = gain * tailLevel t * fromIntegral (tailLeft t - i) / fromIntegral fade
                            addFrame out (off + i) (l * a) (r * a)
                            go (i + 1)
                go 0
                return t { tailPos = tailPos t + count, tailLeft = tailLeft t - run }
        ts <- mapM one (vsTails v)
        let alive = forceSpine [ t | t <- ts, tailLeft t > 0, tailPos t < V.length (tailFrames t) ]
        return v { vsTails = alive }

addFrame :: Ptr Float -> Int -> Double -> Double -> IO ()
addFrame out i l r = do
    ol <- peekElemOff out (2 * i)
    or' <- peekElemOff out (2 * i + 1)
    pokeElemOff out (2 * i) (ol + realToFrac l)
    pokeElemOff out (2 * i + 1) (or' + realToFrac r)
{-# INLINE addFrame #-}

-- | Note what is about to be rendered at an output frame, unless it simply
-- continues the previous segment.
record :: Word64 -> VoiceState -> VoiceState
record outFrame v = case vsHistory v of
    (s : _) | segGen s == vsGen v && segPlaying s == playing && positionIn s outFrame == pos -> v
    hist -> v { vsHistory = Segment outFrame pos playing (vsGen v) : hist }
  where
    pos = vmPosition (vsModel v)
    playing = vmPlaying (vsModel v)

positionIn :: Segment -> Word64 -> Word64
positionIn s outFrame
    | segPlaying s = segPos s + (outFrame - segOut s)
    | otherwise    = segPos s

-- | Drop history that ended before the given output frame.
prune :: Word64 -> VoiceState -> VoiceState
prune from v = v { vsHistory = forceSpine (go (vsHistory v)) }
  where
    go (s : rest)
        | segOut s <= from = [s]
        | otherwise = s : go rest
    go [] = []

-- | What is heard when the device has played the given number of frames.
-- Until a load or seek becomes audible, its target position is reported.
heardAt :: Word64 -> VoiceState -> Heard
heardAt played v = case dropWhile ((> played) . segOut) (vsHistory v) of
    (s : _) | segGen s == vsGen v ->
        Heard (within (positionIn s played)) (segPlaying s)
    _ -> Heard (within (vsGenStart v)) (vmPlaying (vsModel v))
  where
    within = min (vmLength (vsModel v))

-- | Output frames rendered so far.
mixerRenderedFrames :: Mixer -> IO Word64
mixerRenderedFrames m = msRendered <$> currentState m

-- | Wrap a mixer as an 'AudioBackend'. The driver supplies its description,
-- frames queued ahead, frames played so far, device loss, underrun count,
-- and shutdown. After 'backendClose' every operation is a no-op and status
-- reports an empty voice.
mixerBackend :: Mixer -> String -> Int -> IO Word64 -> IO Bool -> IO Int -> IO () -> IO AudioBackend
mixerBackend m description latency played lost underruns closeDriver = do
    closed <- newIORef False
    let fade = mixFade m
        whenOpen act = readIORef closed >>= \c -> unless c act
        update v f = whenOpen (enqueue m (onVoice v f))
        observe :: Voice -> (VoiceState -> a) -> IO a
        observe v f = do
            c <- readIORef closed
            if c then return (f emptyVoice) else f . voiceState v <$> currentState m
    return AudioBackend
        { backendDescription   = description
        , backendSampleRate    = mixerSampleRate m
        , backendLoad          = \v frames -> update v (loadUpdate fade frames)
        , backendReplace       = \v frames -> update v (replaceUpdate fade frames)
        , backendSetPlaying    = \v p -> update v (playingUpdate fade p)
        , backendSeek          = \v pos -> update v (seekUpdate fade pos)
        , backendSetLoop       = \v l -> update v (\s -> s { vsModel = setLoopModel l (vsModel s) })
        , backendSetGain       = \v g -> update v (\s -> s { vsGain = max 0 g })
        , backendSetMasterGain = \g -> whenOpen (enqueue m (\s -> s { msMasterTarget = max 0 g }))
        , backendStatus        = \v -> observe v (voiceModelStatus . vsModel)
        , backendHeard         = \v -> do
            p <- played
            observe v (heardAt p)
        , backendDeviceLost    = lost
        , backendLatencyFrames = latency
        , backendUnderruns     = underruns
        , backendClose         = do
            wasClosed <- atomicModifyIORef' closed (\c -> (True, c))
            unless wasClosed $ do
                closeDriver
                writeIORef (mixShared m) (Shared emptyState [])  -- release buffers
        }

mixerFrames :: Mixer -> Voice -> IO (V.Vector Frame)
mixerFrames m v = vsFrames . voiceState v <$> currentState m

mixerTargetGain :: Mixer -> Voice -> IO Double
mixerTargetGain m v = vsGain . voiceState v <$> currentState m

mixerMasterGain :: Mixer -> IO Double
mixerMasterGain m = msMasterTarget <$> currentState m
