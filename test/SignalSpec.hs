{-# OPTIONS_GHC -Wno-orphans #-}
module SignalSpec (signalTests) where

import Control.Exception (evaluate)
import System.Mem (performMajorGC)
import Test.QuickCheck
import qualified Data.Vector.Storable as V
import Sonora.Music.Composition (Control (..), Music (..), linearize)
import Sonora.Music.Duration (BPM (..), eighth, half, quarter)
import Sonora.Music.Interval (Interval (..))
import Sonora.Music.Note (Articulation (..), Note (..), fff, mf, pp)
import Sonora.Music.Pitch (a5, c3, e4, g4)
import Sonora.Signal.Core
import Sonora.Signal.Effects (chainEffects)
import Sonora.Signal.Envelope
import Sonora.Signal.Filter
import Sonora.Signal.Oscillator
import Sonora.Synth.Instrument (Instrument (..), bassSynth, leadSynth, pluckSynth)
import Sonora.Synth.Synthesizer (masterLimiter, renderComposition, renderNoteEvent)

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

-- | Rendering a composition equals padding every note with silence up to its
-- start and mixing them one after another.
prop_compositionMatchesReference :: Property
prop_compositionMatchesReference = forAll genMusic $ \music ->
    forAll (elements [leadSynth, bassSynth, pluckSynth { instEffects = [] }]) $ \inst ->
        let cfg = AudioConfig 2000 2
            bpm = BPM 240
            notes = map (renderNoteEvent bpm inst) (linearize noteDuration music)
            reference = masterLimiter (chainEffects (instEffects inst) (foldl mixSignals emptySignal notes))
        in counterexample (show music) $
            runSignal (renderComposition bpm inst music) cfg == runSignal reference cfg

-- | Gaps between notes are silent even when the memory they are rendered into
-- was used before.
testGapsAreSilent :: IO Bool
testGapsAreSilent = and <$> mapM attempt [1 .. 20 :: Int]
  where
    inst = pluckSynth { instEffects = [] }
    music = Sequential (Prim (Note c3 eighth mf Normal)) (Sequential (Rest 8) (Prim (Note c3 eighth mf Normal)))
    cfg = AudioConfig 2000 2
    attempt k = do
        -- Leave used memory of many sizes behind for the renderer to reuse.
        used <- mapM (evaluate . garbage) [ n * 1000 + k | n <- [1 .. 60] ]
        _ <- evaluate (sum (map V.length used))
        performMajorGC
        out <- evaluate (runSignal (renderComposition (BPM 240) inst music) cfg)
        return (V.all (== mempty) (V.slice 2000 10000 out))

garbage :: Int -> V.Vector Frame
garbage n = V.replicate n (Frame 7 7)
{-# NOINLINE garbage #-}

instance Show Instrument where
    show = instName

-- | Music with overlaps, rests, repeats of a start time, and nested controls.
genMusic :: Gen (Music Note)
genMusic = sized go
  where
    go n | n <= 4 = leaf
         | otherwise = oneof
            [ leaf
            , Sequential <$> go (n `div` 3) <*> go (n `div` 3)
            , Parallel <$> go (n `div` 3) <*> go (n `div` 3)
            , Modify <$> control <*> go (n `div` 2)
            ]
    leaf = oneof
        [ Rest <$> dur
        , Prim <$> (Note <$> elements [c3, e4, g4, a5] <*> dur <*> elements [pp, mf, fff] <*> elements [Staccato, Normal, Tenuto, Legato]) ]
    dur = elements [0, eighth, quarter, half]
    control = elements
        [ SetTempo (BPM 120), SetTempo (BPM 500), Transpose (Interval 7), ScaleVolume 0.5
        , SetInstrument "chime", SetPan (-0.8) ]

signalTests :: IO Bool
signalTests = do
    putStrLn "--- Running DSP & Signal Processing Tests ---"
    r1 <- quickCheckResult prop_oscillatorBounded
    r2 <- quickCheckResult prop_scaleIdentity
    r3 <- quickCheckResult prop_mixEmptyIdentity
    putStrLn $ "  ADSR Termination Test:    " ++ if testEnvelopeTerminates then "PASSED" else "FAILED"
    putStrLn $ "  Filter Stability Test:    " ++ if testFilterStability then "PASSED" else "FAILED"
    r4 <- quickCheckResult prop_compositionMatchesReference
    gaps <- testGapsAreSilent
    putStrLn $ "  Gaps Are Silent Test:     " ++ if gaps then "PASSED" else "FAILED"
    let allPassed = gaps && all isSuccess [r1, r2, r3, r4] && testEnvelopeTerminates && testFilterStability
    return allPassed
