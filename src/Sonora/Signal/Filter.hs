{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Signal.Filter
    ( FilterType (..)
    , FilterConfig (..)
    , defaultFilterConfig
    , applyFilter
    , lowpass
    , highpass
    , bandpass
    , notch
    , applyModulatedFilter
    ) where

import Control.Monad.ST (runST)
import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Sonora.Signal.Core

-- | Filter modes provided by the State-Variable Filter (SVF).
data FilterType
    = LowPass
    | HighPass
    | BandPass
    | Notch
    deriving (Eq, Show, Read, Generic)

-- | Filter parameters.
data FilterConfig = FilterConfig
    { filterType      :: !FilterType
    , filterCutoff    :: !Double     -- Cutoff frequency in Hz (20 to 20000)
    , filterResonance :: !Double     -- Q factor (0.5 = critically damped, 1.0 = moderate, 10.0+ = high resonance)
    } deriving (Eq, Show, Generic)

defaultFilterConfig :: FilterConfig
defaultFilterConfig = FilterConfig
    { filterType      = LowPass
    , filterCutoff    = 2000.0
    , filterResonance = 0.7071       -- Butterworth Q
    }

-- | Internal filter state for one audio channel.
data SVFState = SVFState
    {-# UNPACK #-} !Double  -- s1
    {-# UNPACK #-} !Double  -- s2

initialSVFState :: SVFState
initialSVFState = SVFState 0.0 0.0

-- | Process a single sample through the Cytomic trapezoidal SVF.
stepSVF :: FilterType -> Double -> Double -> SVFState -> Double -> (Double, SVFState)
stepSVF fType !g !k (SVFState s1 s2) !v0 =
    let !denom = 1.0 + g * (g + k)
        !v3    = v0 - s2
        !v1    = (s1 + g * v3) / denom
        !v2    = s2 + g * v1
        !s1'   = 2.0 * v1 - s1
        !s2'   = 2.0 * v2 - s2
        !out   = case fType of
            LowPass  -> v2
            BandPass -> v1
            HighPass -> v0 - k * v1 - v2
            Notch    -> v0 - k * v1
    in (out, SVFState s1' s2')
{-# INLINE stepSVF #-}

-- | Apply a static filter configuration to a Signal.
applyFilter :: FilterConfig -> Signal -> Signal
applyFilter (FilterConfig fType cutoff res) sig = Signal $ \cfg ->
    let !sr        = fromIntegral (configSampleRate cfg) :: Double
        !clampedFc = max 20.0 (min cutoff (sr * 0.49))
        !q         = max 0.1 res
        !g         = tan (pi * clampedFc / sr)
        !k         = 1.0 / q
        inVec      = runSignal sig cfg
        len        = V.length inVec
    in runST $ do
        outBuf <- MV.new len
        let loop !i !stL !stR
                | i >= len = return ()
                | otherwise = do
                    let Frame inL inR = inVec V.! i
                        !(outL, stL') = stepSVF fType g k stL inL
                        !(outR, stR') = stepSVF fType g k stR inR
                    MV.write outBuf i (Frame outL outR)
                    loop (i + 1) stL' stR'
        loop 0 initialSVFState initialSVFState
        V.freeze outBuf

-- | Apply a dynamic filter where cutoff frequency changes over time (e.g. modulated by an envelope).
applyModulatedFilter :: FilterType -> Double -> V.Vector Double -> Signal -> Signal
applyModulatedFilter fType res cutoffVec sig = Signal $ \cfg ->
    let !sr      = fromIntegral (configSampleRate cfg) :: Double
        !q       = max 0.1 res
        !k       = 1.0 / q
        inVec    = runSignal sig cfg
        totalLen = min (V.length inVec) (V.length cutoffVec)
    in runST $ do
        outBuf <- MV.new totalLen
        let loop !idx !stL !stR
                | idx >= totalLen = return ()
                | otherwise = do
                    let !fcRaw = cutoffVec V.! idx
                        !fc    = max 20.0 (min fcRaw (sr * 0.49))
                        !g     = tan (pi * fc / sr)
                        Frame inL inR = inVec V.! idx
                        !(outL, stL') = stepSVF fType g k stL inL
                        !(outR, stR') = stepSVF fType g k stR inR
                    MV.write outBuf idx (Frame outL outR)
                    loop (idx + 1) stL' stR'
        loop 0 initialSVFState initialSVFState
        V.freeze outBuf

-- Convenient helper functions
lowpass :: Double -> Double -> Signal -> Signal
lowpass fc q = applyFilter (FilterConfig LowPass fc q)

highpass :: Double -> Double -> Signal -> Signal
highpass fc q = applyFilter (FilterConfig HighPass fc q)

bandpass :: Double -> Double -> Signal -> Signal
bandpass fc q = applyFilter (FilterConfig BandPass fc q)

notch :: Double -> Double -> Signal -> Signal
notch fc q = applyFilter (FilterConfig Notch fc q)
