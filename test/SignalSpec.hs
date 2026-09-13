module SignalSpec (signalTests) where

import Test.QuickCheck
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core
import Sonora.Signal.Envelope
import Sonora.Signal.Filter
import Sonora.Signal.Oscillator

testCfg :: AudioConfig
testCfg = AudioConfig 44100 2

-- | Property: Oscillators are always bounded within [-1.0, 1.0].
prop_oscillatorBounded :: Property
prop_oscillatorBounded = forAll (choose (50.0, 2000.0)) $ \freq ->
    let sig = sineWave freq 0.9 0.05
        frames = runSignal sig testCfg
        isBounded (Frame l r) = abs l <= 1.0001 && abs r <= 1.0001
    in V.all isBounded frames

-- | Property: Scaling a signal by 1.0 preserves exact sample frames.
prop_scaleIdentity :: Property
prop_scaleIdentity = forAll (choose (100.0, 1000.0)) $ \freq ->
    let sig1 = sineWave freq 0.8 0.02
        sigScaled = scaleSignal 1.0 sig1
        v1 = runSignal sig1 testCfg
        v2 = runSignal sigScaled testCfg
    in v1 == v2

-- | Property: Mixing with emptySignal is identity.
prop_mixEmptyIdentity :: Property
prop_mixEmptyIdentity = forAll (choose (100.0, 1000.0)) $ \freq ->
    let sig = sawWave freq 0.7 0.02
        mixed = mixSignals emptySignal sig
        v1 = runSignal sig testCfg
        v2 = runSignal mixed testCfg
    in v1 == v2

-- | Unit test: ADSR envelope terminates at 0.0 after gate + release.
testEnvelopeTerminates :: Bool
testEnvelopeTerminates =
    let env = adsr 0.01 0.02 0.5 0.03
        gateDur = 0.10
        valAtEnd = evalADSR env gateDur (gateDur + 0.031)
    in valAtEnd == 0.0

-- | Unit test: Cytomic SVF filter is numerically stable and never produces NaNs or Infinities.
testFilterStability :: Bool
testFilterStability =
    let osc = sawWave 440.0 0.9 0.1
        filtered = lowpass 1200.0 4.0 osc
        frames = runSignal filtered testCfg
        isValidDouble x = not (isNaN x || isInfinite x)
        isValidFrame (Frame l r) = isValidDouble l && isValidDouble r
    in V.all isValidFrame frames

signalTests :: IO Bool
signalTests = do
    putStrLn "--- Running DSP & Signal Processing Tests ---"
    r1 <- quickCheckResult prop_oscillatorBounded
    r2 <- quickCheckResult prop_scaleIdentity
    r3 <- quickCheckResult prop_mixEmptyIdentity
    putStrLn $ "  ADSR Termination Test:    " ++ if testEnvelopeTerminates then "PASSED" else "FAILED"
    putStrLn $ "  Filter Stability Test:    " ++ if testFilterStability then "PASSED" else "FAILED"
    let allPassed = all isSuccess [r1, r2, r3] && testEnvelopeTerminates && testFilterStability
    return allPassed
