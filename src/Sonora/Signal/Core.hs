{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Signal.Core
    ( AudioConfig (..)
    , defaultConfig
    , Frame (..)
    , monoFrame
    , stereoFrame
    , panFrame
    , mixFrames
    , scaleFrame
    , Signal (..)
    , emptySignal
    , constantSignal
    , mapSignal
    , zipWithSignal
    , mixSignals
    , scaleSignal
    , delaySignal
    , concatSignals
    , signalDuration
    ) where

import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V

-- | Audio rendering configuration.
data AudioConfig = AudioConfig
    { configSampleRate :: !Int
    , configChannels   :: !Int  -- 1 = Mono, 2 = Stereo
    } deriving (Eq, Show, Generic)

defaultConfig :: AudioConfig
defaultConfig = AudioConfig
    { configSampleRate = 44100
    , configChannels   = 2
    }

-- | Stereo audio sample frame containing Left and Right channel values (-1.0 to 1.0).
data Frame = Frame
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Double
    deriving (Eq, Show, Generic)

instance Storable Frame where
    sizeOf _ = 16
    alignment _ = 8
    peek ptr = do
        !l <- peekByteOff ptr 0
        !r <- peekByteOff ptr 8
        return (Frame l r)
    poke ptr (Frame !l !r) = do
        pokeByteOff ptr 0 l
        pokeByteOff ptr 8 r

instance Semigroup Frame where
    {-# INLINE (<>) #-}
    Frame l1 r1 <> Frame l2 r2 = Frame (l1 + l2) (r1 + r2)

instance Monoid Frame where
    {-# INLINE mempty #-}
    mempty = Frame 0.0 0.0

instance Num Frame where
    {-# INLINE (+) #-}
    Frame l1 r1 + Frame l2 r2 = Frame (l1 + l2) (r1 + r2)
    {-# INLINE (-) #-}
    Frame l1 r1 - Frame l2 r2 = Frame (l1 - l2) (r1 - r2)
    {-# INLINE (*) #-}
    Frame l1 r1 * Frame l2 r2 = Frame (l1 * l2) (r1 * r2)
    abs (Frame l r)           = Frame (abs l) (abs r)
    signum (Frame l r)        = Frame (signum l) (signum r)
    fromInteger n             = let !x = fromInteger n in Frame x x

-- | Create a mono frame (equal left and right).
monoFrame :: Double -> Frame
monoFrame !x = Frame x x

-- | Create an explicit stereo frame.
stereoFrame :: Double -> Double -> Frame
stereoFrame !l !r = Frame l r

-- | Constant-power stereo panning from mono sample.
-- Pan position is from -1.0 (hard left) to +1.0 (hard right).
panFrame :: Double -> Double -> Frame
panFrame !pan !sample =
    let !clamped = max (-1.0) (min 1.0 pan)
        -- Map -1.0..1.0 to angle 0..pi/2
        !angle   = (clamped + 1.0) * (pi / 4.0)
        !leftGain  = cos angle
        !rightGain = sin angle
    in Frame (sample * leftGain) (sample * rightGain)

-- | Mix two frames together.
mixFrames :: Frame -> Frame -> Frame
mixFrames = (<>)

-- | Scale a frame by a scalar amplitude factor.
scaleFrame :: Double -> Frame -> Frame
scaleFrame !s (Frame l r) = Frame (l * s) (r * s)

-- | A discrete audio signal is a function from audio configuration to an unboxed storable buffer of frames.
newtype Signal = Signal { runSignal :: AudioConfig -> V.Vector Frame }

instance Semigroup Signal where
    (<>) = mixSignals

instance Monoid Signal where
    mempty = emptySignal

-- | An empty (silent, zero length) signal.
emptySignal :: Signal
emptySignal = Signal $ \_ -> V.empty

-- | A signal containing a constant frame for a given number of samples.
constantSignal :: Frame -> Int -> Signal
constantSignal frame len = Signal $ \_ -> V.replicate (max 0 len) frame

-- | Map a pure function over every frame in a signal.
mapSignal :: (Frame -> Frame) -> Signal -> Signal
mapSignal f sig = Signal $ \cfg -> V.map f (runSignal sig cfg)

-- | Combine two signals sample-by-sample.
zipWithSignal :: (Frame -> Frame -> Frame) -> Signal -> Signal -> Signal
zipWithSignal f sig1 sig2 = Signal $ \cfg ->
    let v1 = runSignal sig1 cfg
        v2 = runSignal sig2 cfg
        len = max (V.length v1) (V.length v2)
    in V.generate len $ \i ->
        let f1 = if i < V.length v1 then v1 V.! i else mempty
            f2 = if i < V.length v2 then v2 V.! i else mempty
        in f f1 f2

-- | Mix two signals in parallel. If durations differ, pads the shorter with silence.
mixSignals :: Signal -> Signal -> Signal
mixSignals = zipWithSignal (<>)

-- | Scale the amplitude of a signal by a factor.
scaleSignal :: Double -> Signal -> Signal
scaleSignal factor = mapSignal (scaleFrame factor)

-- | Delay a signal by a given number of silence samples at the beginning.
delaySignal :: Int -> Signal -> Signal
delaySignal numSamples sig
    | numSamples <= 0 = sig
    | otherwise       = Signal $ \cfg ->
        let silence = V.replicate numSamples mempty
            v       = runSignal sig cfg
        in silence V.++ v

-- | Concatenate two signals sequentially.
concatSignals :: Signal -> Signal -> Signal
concatSignals sig1 sig2 = Signal $ \cfg ->
    runSignal sig1 cfg V.++ runSignal sig2 cfg

-- | Total duration of a signal in seconds given the audio configuration.
signalDuration :: AudioConfig -> Signal -> Double
signalDuration cfg sig =
    let numSamples = V.length (runSignal sig cfg)
    in fromIntegral numSamples / fromIntegral (configSampleRate cfg)
