{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Signal.Oscillator
    ( Waveform (..)
    , OscillatorConfig (..)
    , defaultOscConfig
    -- Generator functions returning Signal
    , oscillator
    , sineWave
    , squareWave
    , triangleWave
    , sawWave
    , whiteNoise
    , pinkNoise
    -- Raw sample generators for custom synthesis
    , sineSample
    , squareSample
    , triangleSample
    , sawSample
    ) where

import Data.Bits (xor, shiftR, shiftL)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core

-- | Fundamental oscillator waveform shapes.
data Waveform
    = Sine
    | Square !Double      -- Duty cycle (e.g. 0.5 for square, 0.1 for pulse)
    | Triangle
    | Sawtooth
    | WhiteNoiseGen
    | PinkNoiseGen
    deriving (Eq, Show, Read, Generic)

-- | Configuration parameters for an oscillator.
data OscillatorConfig = OscillatorConfig
    { oscWaveform  :: !Waveform
    , oscFrequency :: !Double     -- Hertz
    , oscAmplitude :: !Double     -- 0.0 to 1.0
    , oscPhase     :: !Double     -- Radians (0 to 2*pi)
    , oscDuration  :: !Double     -- Seconds
    } deriving (Eq, Show, Generic)

defaultOscConfig :: OscillatorConfig
defaultOscConfig = OscillatorConfig
    { oscWaveform  = Sine
    , oscFrequency = 440.0
    , oscAmplitude = 0.8
    , oscPhase     = 0.0
    , oscDuration  = 1.0
    }

-- | Compute sine wave sample at normalized phase in [0, 1).
sineSample :: Double -> Double
sineSample !phase01 = sin (phase01 * 2.0 * pi)
{-# INLINE sineSample #-}

-- | Compute square wave sample at normalized phase with given duty cycle.
squareSample :: Double -> Double -> Double
squareSample !duty !phase01 =
    if phase01 < duty then 1.0 else -1.0
{-# INLINE squareSample #-}

-- | Compute triangle wave sample at normalized phase in [0, 1).
-- Ramp up from -1 to +1 on [0, 0.5], down to -1 on [0.5, 1].
triangleSample :: Double -> Double
triangleSample !phase01 =
    let !p = phase01 - fromIntegral (floor phase01 :: Int)
    in if p < 0.5
       then 4.0 * p - 1.0
       else 3.0 - 4.0 * p
{-# INLINE triangleSample #-}

-- | Compute sawtooth wave sample at normalized phase in [0, 1).
sawSample :: Double -> Double
sawSample !phase01 =
    let !p = phase01 - fromIntegral (floor phase01 :: Int)
    in 2.0 * p - 1.0
{-# INLINE sawSample #-}

-- | Generate an audio signal from an oscillator configuration.
oscillator :: OscillatorConfig -> Signal
oscillator (OscillatorConfig wave freq amp initPhase dur) = Signal $ \cfg ->
    let sr = fromIntegral (configSampleRate cfg) :: Double
        numSamples = max 0 (round (dur * sr))
        phaseStep  = freq / sr
        startPhase = initPhase / (2.0 * pi)
    in case wave of
        Sine ->
            V.generate numSamples $ \i ->
                let !phase01 = startPhase + fromIntegral i * phaseStep
                    !sample  = amp * sineSample phase01
                in monoFrame sample

        Square duty ->
            V.generate numSamples $ \i ->
                let !phase01 = startPhase + fromIntegral i * phaseStep
                    !pNorm   = phase01 - fromIntegral (floor phase01 :: Int)
                    !sample  = amp * squareSample duty pNorm
                in monoFrame sample

        Triangle ->
            V.generate numSamples $ \i ->
                let !phase01 = startPhase + fromIntegral i * phaseStep
                    !sample  = amp * triangleSample phase01
                in monoFrame sample

        Sawtooth ->
            V.generate numSamples $ \i ->
                let !phase01 = startPhase + fromIntegral i * phaseStep
                    !sample  = amp * sawSample phase01
                in monoFrame sample

        WhiteNoiseGen ->
            generateWhiteNoise numSamples amp

        PinkNoiseGen ->
            generatePinkNoise numSamples amp

-- | Pure deterministic PRNG for white noise (xorshift64*).
generateWhiteNoise :: Int -> Double -> V.Vector Frame
generateWhiteNoise numSamples amp =
    let initSeed = 0x853c49e6748fea9b :: Word64
        stepSeed !s =
            let !s1 = s `xor` (s `shiftL` 13)
                !s2 = s1 `xor` (s1 `shiftR` 7)
                !s3 = s2 `xor` (s2 `shiftL` 17)
            in s3
        sampleOf !s =
            -- Map Word64 to [-1.0, 1.0]
            let !norm = (fromIntegral s :: Double) / 18446744073709551615.0
            in (norm * 2.0 - 1.0) * amp
    in V.unfoldrExactN numSamples (\s ->
        let !s' = stepSeed s
            !val = sampleOf s'
        in (monoFrame val, s')) initSeed

-- | Pure deterministic 3-pole pink noise generator (Paul Kellet's algorithm).
-- Creates 1/f spectral slope rolling off at ~3dB per octave.
generatePinkNoise :: Int -> Double -> V.Vector Frame
generatePinkNoise numSamples amp =
    let initSeed = 0x9e3779b97f4a7c15 :: Word64
        stepSeed !s =
            let !s1 = s `xor` (s1 `shiftL` 13)
                !s2 = s1 `xor` (s1 `shiftR` 7)
                !s3 = s2 `xor` (s2 `shiftL` 17)
            in s3
        stepFilter (!b0, !b1, !b2, !s) =
            let !s'   = stepSeed s
                !norm = (fromIntegral s' :: Double) / 18446744073709551615.0
                !white = norm * 2.0 - 1.0
                !nb0  = 0.99886 * b0 + white * 0.0555179
                !nb1  = 0.99332 * b1 + white * 0.0750759
                !nb2  = 0.96900 * b2 + white * 0.1538520
                !pink = (nb0 + nb1 + nb2 + white * 0.5362) * 0.11 * amp
            in (monoFrame pink, (nb0, nb1, nb2, s'))
    in V.unfoldrExactN numSamples stepFilter (0.0, 0.0, 0.0, initSeed)

-- Convenient helper constructors
sineWave :: Double -> Double -> Double -> Signal
sineWave freq amp dur = oscillator (defaultOscConfig { oscWaveform = Sine, oscFrequency = freq, oscAmplitude = amp, oscDuration = dur })

squareWave :: Double -> Double -> Double -> Double -> Signal
squareWave freq duty amp dur = oscillator (defaultOscConfig { oscWaveform = Square duty, oscFrequency = freq, oscAmplitude = amp, oscDuration = dur })

triangleWave :: Double -> Double -> Double -> Signal
triangleWave freq amp dur = oscillator (defaultOscConfig { oscWaveform = Triangle, oscFrequency = freq, oscAmplitude = amp, oscDuration = dur })

sawWave :: Double -> Double -> Double -> Signal
sawWave freq amp dur = oscillator (defaultOscConfig { oscWaveform = Sawtooth, oscFrequency = freq, oscAmplitude = amp, oscDuration = dur })

whiteNoise :: Double -> Double -> Signal
whiteNoise amp dur = oscillator (defaultOscConfig { oscWaveform = WhiteNoiseGen, oscAmplitude = amp, oscDuration = dur })

pinkNoise :: Double -> Double -> Signal
pinkNoise amp dur = oscillator (defaultOscConfig { oscWaveform = PinkNoiseGen, oscAmplitude = amp, oscDuration = dur })
