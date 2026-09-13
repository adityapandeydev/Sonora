{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Signal.Effects
    ( -- Core transformations
      gain
    , pan
    -- Time-based effects
    , delay
    , echo
    , schroederReverb
    , simpleReverb
    -- Non-linear / Modulation
    , distortion
    , tremolo
    , chorus
    -- Effect chaining
    , chainEffects
    ) where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.ST (runST)
import Sonora.Signal.Core

-- | Scale the signal gain by a scalar multiplier.
gain :: Double -> Signal -> Signal
gain = scaleSignal

-- | Pan a stereo signal (-1.0 = hard left, 0.0 = center, 1.0 = hard right).
pan :: Double -> Signal -> Signal
pan panPos sig = Signal $ \cfg ->
    let clamped = max (-1.0) (min 1.0 panPos)
        angle   = (clamped + 1.0) * (pi / 4.0)
        gainL   = cos angle * sqrt 2.0
        gainR   = sin angle * sqrt 2.0
        inVec   = runSignal sig cfg
    in V.map (\(Frame l r) -> Frame (l * gainL) (r * gainR)) inVec

-- | Delay effect with feedback and wet/dry mix.
-- delayTime in seconds, feedback in [0, 1), wetDry in [0, 1] (0 = dry, 1 = wet).
delay :: Double -> Double -> Double -> Signal -> Signal
delay delayTimeSec feedback wetDry sig = Signal $ \cfg ->
    let sr = configSampleRate cfg
        delaySamples = max 1 (round (delayTimeSec * fromIntegral sr))
        fb  = max 0.0 (min 0.95 feedback)
        wet = max 0.0 (min 1.0 wetDry)
        dry = 1.0 - wet
        inVec = runSignal sig cfg
        inLen = V.length inVec
        -- Add tail so delay doesn't abruptly cut off
        extraTail = round (delayTimeSec * 4.0 * fromIntegral sr)
        outLen = inLen + extraTail

    in runST $ do
        outBuf <- MV.replicate outLen mempty
        delayBufL <- MV.replicate delaySamples (0.0 :: Double)
        delayBufR <- MV.replicate delaySamples (0.0 :: Double)

        let loop !i !dIdx
                | i >= outLen = return ()
                | otherwise = do
                    let (inL, inR) = if i < inLen
                                     then case inVec V.! i of Frame l r -> (l, r)
                                     else (0.0, 0.0)

                    dOutL <- MV.read delayBufL dIdx
                    dOutR <- MV.read delayBufR dIdx

                    let !newDelayL = inL + dOutL * fb
                        !newDelayR = inR + dOutR * fb

                    MV.write delayBufL dIdx newDelayL
                    MV.write delayBufR dIdx newDelayR

                    let !mixL = inL * dry + dOutL * wet
                        !mixR = inR * dry + dOutR * wet

                    MV.write outBuf i (Frame mixL mixR)

                    let !nextDIdx = (dIdx + 1) `rem` delaySamples
                    loop (i + 1) nextDIdx

        loop 0 0
        V.freeze outBuf

-- | Multi-tap echo with distinct decay taps.
echo :: [(Double, Double)] -> Signal -> Signal
echo taps sig = Signal $ \cfg ->
    let base = runSignal sig cfg
        sr   = fromIntegral (configSampleRate cfg) :: Double
        delayTap (timeSec, tapGain) =
            let numSamples = round (timeSec * sr)
                v = V.replicate numSamples mempty V.++ V.map (scaleFrame tapGain) base
            in v
        allTaps = base : map delayTap taps
        maxLen  = maximum (map V.length allTaps)
    in V.generate maxLen $ \i ->
        foldl (\acc vec ->
            if i < V.length vec
            then acc <> (vec V.! i)
            else acc) mempty allTaps

-- | Pure Schroeder Reverb implementation:
-- 4 Parallel Feedback Comb Filters -> 2 Series All-Pass Filters.
schroederReverb :: Double -> Double -> Signal -> Signal
schroederReverb roomSize wetMix sig = Signal $ \cfg ->
    let sr = fromIntegral (configSampleRate cfg) :: Double
        scaleRatio = sr / 44100.0

        -- Delay lengths in samples tuned to reduce resonance peaks
        combDelays = map (round . (* scaleRatio)) [1116.0, 1188.0, 1277.0, 1356.0]
        combFeedback = max 0.1 (min 0.95 (0.7 + roomSize * 0.25))

        allpassDelays = map (round . (* scaleRatio)) [225.0, 556.0]
        allpassGain = 0.5 :: Double

        inVec = runSignal sig cfg
        inLen = V.length inVec
        extraTail = round (sr * (1.5 + roomSize * 2.0))
        outLen = inLen + extraTail
        wet = max 0.0 (min 1.0 wetMix)
        dry = 1.0 - wet

    in runST $ do
        -- Process Comb filters
        combOutputs <- mapM (\delayLen -> do
            bufL <- MV.replicate delayLen (0.0 :: Double)
            bufR <- MV.replicate delayLen (0.0 :: Double)
            out  <- MV.replicate outLen mempty
            let loop !i !dIdx
                    | i >= outLen = return ()
                    | otherwise = do
                        let (inL, inR) = if i < inLen
                                         then case inVec V.! i of Frame l r -> (l, r)
                                         else (0.0, 0.0)
                        dL <- MV.read bufL dIdx
                        dR <- MV.read bufR dIdx
                        let !newDL = inL + dL * combFeedback
                            !newDR = inR + dR * combFeedback
                        MV.write bufL dIdx newDL
                        MV.write bufR dIdx newDR
                        MV.write out i (Frame dL dR)
                        loop (i + 1) ((dIdx + 1) `rem` delayLen)
            loop 0 0
            V.freeze out) combDelays

        -- Sum comb outputs
        let summedCombs = V.generate outLen $ \i ->
                let f1 = (combOutputs !! 0) V.! i
                    f2 = (combOutputs !! 1) V.! i
                    f3 = (combOutputs !! 2) V.! i
                    f4 = (combOutputs !! 3) V.! i
                in scaleFrame 0.25 (f1 + f2 + f3 + f4)

        -- Pass through Allpass filters in series
        let runAllpass inV delayLen = runST $ do
                bufL <- MV.replicate delayLen (0.0 :: Double)
                bufR <- MV.replicate delayLen (0.0 :: Double)
                out  <- MV.replicate outLen mempty
                let loop !i !dIdx
                        | i >= outLen = return ()
                        | otherwise = do
                            let Frame xL xR = inV V.! i
                            bufValL <- MV.read bufL dIdx
                            bufValR <- MV.read bufR dIdx
                            let !yL = (-allpassGain * xL) + bufValL
                                !yR = (-allpassGain * xR) + bufValR
                                !newBufL = xL + (allpassGain * yL)
                                !newBufR = xR + (allpassGain * yR)
                            MV.write bufL dIdx newBufL
                            MV.write bufR dIdx newBufR
                            MV.write out i (Frame yL yR)
                            loop (i + 1) ((dIdx + 1) `rem` delayLen)
                loop 0 0
                V.freeze out

        let ap1 = runAllpass summedCombs (allpassDelays !! 0)
            ap2 = runAllpass ap1 (allpassDelays !! 1)

        -- Mix wet and dry
        return $ V.generate outLen $ \i ->
            let dryFrame = if i < inLen then inVec V.! i else mempty
                wetFrame = ap2 V.! i
            in scaleFrame dry dryFrame + scaleFrame wet wetFrame

-- | Simple one-dial reverb preset.
simpleReverb :: Double -> Signal -> Signal
simpleReverb wet = schroederReverb 0.5 wet

-- | Smooth soft-clipping distortion using tanh saturation.
-- Drive >= 1.0 (e.g. 1.0 = clean/light, 4.0 = crunchy, 10.0+ = high gain fuzz).
distortion :: Double -> Signal -> Signal
distortion drive sig = Signal $ \cfg ->
    let !d = max 1.0 drive
        inVec = runSignal sig cfg
        saturate !x = tanh (x * d) / tanh d
    in V.map (\(Frame l r) -> Frame (saturate l) (saturate r)) inVec

-- | Tremolo effect (amplitude modulation using low frequency oscillator).
-- rate: LFO speed in Hz (e.g. 4.0 Hz), depth: modulation intensity (0.0 to 1.0).
tremolo :: Double -> Double -> Signal -> Signal
tremolo rate depth sig = Signal $ \cfg ->
    let sr = fromIntegral (configSampleRate cfg) :: Double
        d  = max 0.0 (min 1.0 depth)
        inVec = runSignal sig cfg
        len = V.length inVec
    in V.generate len $ \i ->
        let !t = fromIntegral i / sr
            !lfo = (sin (2.0 * pi * rate * t) + 1.0) * 0.5  -- 0.0 to 1.0
            !modGain = 1.0 - d * (1.0 - lfo)
            Frame l r = inVec V.! i
        in Frame (l * modGain) (r * modGain)

-- | Stereo Chorus effect: creates thickness by modulating delay lines with LFOs.
chorus :: Double -> Double -> Double -> Signal -> Signal
chorus rate depth wetDry sig = Signal $ \cfg ->
    let sr = fromIntegral (configSampleRate cfg) :: Double
        inVec = runSignal sig cfg
        len   = V.length inVec
        baseDelayMs = 0.020 -- 20 ms base delay
        depthMs     = 0.005 * max 0.0 (min 1.0 depth)
        wet = max 0.0 (min 1.0 wetDry)
        dry = 1.0 - wet
        maxDelaySamples = round ((baseDelayMs + depthMs + 0.01) * sr)

    in runST $ do
        bufL <- MV.replicate maxDelaySamples (0.0 :: Double)
        bufR <- MV.replicate maxDelaySamples (0.0 :: Double)
        out  <- MV.replicate len mempty

        let loop !i !bufIdx
                | i >= len = return ()
                | otherwise = do
                    let !t = fromIntegral i / sr
                        Frame inL inR = inVec V.! i

                    MV.write bufL bufIdx inL
                    MV.write bufR bufIdx inR

                    -- Modulate delay with 90 degree phase shift between L and R for stereo width
                    let !modL = sin (2.0 * pi * rate * t)
                        !modR = cos (2.0 * pi * rate * t)
                        !delTimeL = baseDelayMs + depthMs * modL
                        !delTimeR = baseDelayMs + depthMs * modR
                        !delSampL = delTimeL * sr
                        !delSampR = delTimeR * sr

                        -- Linear interpolation for fractional delay
                        readTap buf delSamp = do
                            let !intSamp = floor delSamp :: Int
                                !frac    = delSamp - fromIntegral intSamp
                                !idx0    = (bufIdx - intSamp + maxDelaySamples) `rem` maxDelaySamples
                                !idx1    = (idx0 - 1 + maxDelaySamples) `rem` maxDelaySamples
                            s0 <- MV.read buf idx0
                            s1 <- MV.read buf idx1
                            return (s0 * (1.0 - frac) + s1 * frac)

                    tapL <- readTap bufL delSampL
                    tapR <- readTap bufR delSampR

                    let !outL = inL * dry + tapL * wet
                        !outR = inR * dry + tapR * wet

                    MV.write out i (Frame outL outR)
                    loop (i + 1) ((bufIdx + 1) `rem` maxDelaySamples)

        loop 0 0
        V.freeze out

-- | Chain a list of effects sequentially: osc >>> env >>> filter >>> distortion >>> reverb.
chainEffects :: [Signal -> Signal] -> Signal -> Signal
chainEffects = foldr (.) id
