{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Signal.Envelope
    ( EnvelopeCurve (..)
    , ADSR (..)
    , defaultADSR
    , adsr
    , adsrExp
    , evalADSR
    , generateEnvelopeVector
    , applyEnvelope
    , modulateParameter
    -- Common presets
    , organADSR
    , pluckADSR
    , padADSR
    , percussiveADSR
    , pianoADSR
    , stringADSR
    ) where

import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core

-- | Envelope interpolation curvature.
data EnvelopeCurve
    = LinearCurve
    | ExponentialCurve
    deriving (Eq, Show, Read, Generic)

-- | Classic 4-stage Attack-Decay-Sustain-Release envelope definition.
data ADSR = ADSR
    { adsrAttack  :: !Double         -- Attack duration in seconds (>= 0)
    , adsrDecay   :: !Double         -- Decay duration in seconds (>= 0)
    , adsrSustain :: !Double         -- Sustain level (0.0 to 1.0)
    , adsrRelease :: !Double         -- Release duration in seconds (>= 0)
    , adsrCurve   :: !EnvelopeCurve  -- Linear or Exponential
    } deriving (Eq, Show, Read, Generic)

defaultADSR :: ADSR
defaultADSR = ADSR
    { adsrAttack  = 0.02
    , adsrDecay   = 0.10
    , adsrSustain = 0.70
    , adsrRelease = 0.20
    , adsrCurve   = LinearCurve
    }

-- | Smart constructor for linear ADSR.
adsr :: Double -> Double -> Double -> Double -> ADSR
adsr a d s r = ADSR a d s r LinearCurve

-- | Smart constructor for exponential ADSR.
adsrExp :: Double -> Double -> Double -> Double -> ADSR
adsrExp a d s r = ADSR a d s r ExponentialCurve

-- | Evaluate the ADSR envelope at time 't' given total note gate duration 'gateTime'.
evalADSR :: ADSR -> Double -> Double -> Double
evalADSR (ADSR a d s r curve) !gateDuration !t
    | t < 0.0          = 0.0
    | t < a            = -- Attack phase
        let progress = if a <= 0 then 1.0 else t / a
        in case curve of
            LinearCurve      -> progress
            ExponentialCurve -> progress * progress
    | t < a + d        = -- Decay phase
        let progress = if d <= 0 then 1.0 else (t - a) / d
        in case curve of
            LinearCurve      -> 1.0 - progress * (1.0 - s)
            ExponentialCurve -> 1.0 - (progress * (2.0 - progress)) * (1.0 - s)
    | t < gateDuration = -- Sustain phase
        s
    | t < gateDuration + r = -- Release phase
        let progress = if r <= 0 then 1.0 else (t - gateDuration) / r
        in case curve of
            LinearCurve      -> s * (1.0 - progress)
            ExponentialCurve -> s * (1.0 - progress) * (1.0 - progress)
    | otherwise        = 0.0

-- | Generate a vector of envelope multipliers for a given note duration and audio configuration.
generateEnvelopeVector :: ADSR -> Double -> AudioConfig -> V.Vector Double
generateEnvelopeVector env gateDuration cfg =
    let sr = fromIntegral (configSampleRate cfg) :: Double
        totalDur = gateDuration + adsrRelease env
        numSamples = max 0 (round (totalDur * sr))
    in V.generate numSamples $ \i ->
        let !t = fromIntegral i / sr
        in evalADSR env gateDuration t

-- | Apply an ADSR envelope to modulate the amplitude of a Signal.
applyEnvelope :: ADSR -> Double -> Signal -> Signal
applyEnvelope env gateDuration sig = Signal $ \cfg ->
    let vSig = runSignal sig cfg
        vEnv = generateEnvelopeVector env gateDuration cfg
        len  = min (V.length vSig) (V.length vEnv)
    in V.generate len $ \i ->
        let !f   = vSig V.! i
            !envVal = vEnv V.! i
        in scaleFrame envVal f

-- | Modulate any synthesis parameter (e.g. filter cutoff frequency) using an envelope.
modulateParameter :: ADSR -> Double -> Double -> Double -> AudioConfig -> V.Vector Double
modulateParameter env gateDur minVal maxVal cfg =
    let vEnv = generateEnvelopeVector env gateDur cfg
        delta = maxVal - minVal
    in V.map (\e -> minVal + e * delta) vEnv

-- Presets
organADSR :: ADSR
organADSR = ADSR 0.005 0.01 1.0 0.02 LinearCurve

pluckADSR :: ADSR
pluckADSR = ADSR 0.001 0.15 0.0 0.10 ExponentialCurve

padADSR :: ADSR
padADSR = ADSR 0.8 1.2 0.8 1.5 LinearCurve

percussiveADSR :: ADSR
percussiveADSR = ADSR 0.001 0.08 0.0 0.05 ExponentialCurve

pianoADSR :: ADSR
pianoADSR = ADSR 0.01 0.35 0.3 0.4 ExponentialCurve

stringADSR :: ADSR
stringADSR = ADSR 0.3 0.5 0.7 0.6 LinearCurve
