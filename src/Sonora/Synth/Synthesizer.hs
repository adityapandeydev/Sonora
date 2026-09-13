{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonora.Synth.Synthesizer
    ( renderNoteEvent
    , renderComposition
    , masterLimiter
    ) where

import qualified Data.Vector.Storable as V
import Sonora.Music.Composition
import Sonora.Music.Duration (BPM, durationToSeconds, durationToSamples)
import Sonora.Music.Interval (Interval (..), transposePitch)
import Sonora.Music.Note
import Sonora.Music.Pitch (pitchToFreq)
import Sonora.Signal.Core
import Sonora.Signal.Effects (chainEffects, pan)
import Sonora.Synth.Instrument
import Sonora.Synth.Voice

-- | Render a single timed note event to an audio Signal.
renderNoteEvent :: BPM -> Instrument -> TimedEvent Note -> Signal
renderNoteEvent defaultBpm defaultInst (TimedEvent start dur controls Note{..}) =
    let -- Extract active controls from event scope
        activeBpm = foldr (\c b -> case c of SetTempo bpm -> bpm; _ -> b) defaultBpm controls
        activeInst = foldr (\c i -> case c of SetInstrument name -> maybe i id (lookupInstrument name); _ -> i) defaultInst controls
        activeTrans = foldr (\c (Interval t) -> case c of Transpose (Interval st) -> Interval (t + st); _ -> Interval t) (Interval 0) controls
        activeVolScale = foldr (\c v -> case c of ScaleVolume s -> v * s; _ -> v) 1.0 controls
        activePan = foldr (\c pnAcc -> case c of SetPan pn -> pn; _ -> pnAcc) 0.0 controls

        -- Compute pitch frequency after transposition
        transposedPitch = transposePitch activeTrans notePitch
        freq = pitchToFreq transposedPitch

        -- Compute duration in seconds and articulation gate ratio
        totalSec = durationToSeconds activeBpm dur
        gateRatio = case noteArticulation of
            Staccato -> 0.45
            Normal   -> 0.85
            Tenuto   -> 0.95
            Legato   -> 1.00
        gateSec = totalSec * gateRatio

        -- Effective velocity
        effVel = min 1.0 (max 0.0 (getVelocity noteVelocity * activeVolScale))

        -- Synthesize voice
        voiceSig = renderVoice (instVoice activeInst) freq gateSec effVel

        -- Apply panning
        pannedSig = pan activePan voiceSig

    in Signal $ \cfg ->
        let startSamples = durationToSamples (configSampleRate cfg) activeBpm start
            v = runSignal pannedSig cfg
            silence = V.replicate startSamples mempty
        in silence V.++ v

-- | Render an entire musical composition to a master Signal.
renderComposition :: BPM -> Instrument -> Music Note -> Signal
renderComposition bpm inst music =
    let events = linearize noteDuration music
        noteSignals = map (renderNoteEvent bpm inst) events
        mixedNotes = foldl mixSignals emptySignal noteSignals
        -- Apply instrument-level master effect chain
        processed = chainEffects (instEffects inst) mixedNotes
    in masterLimiter processed

-- | Master brickwall/soft limiter to prevent harsh digital clipping.
-- Smoothly compresses peaks exceeding -0.1 dBFS.
masterLimiter :: Signal -> Signal
masterLimiter sig = Signal $ \cfg ->
    let inVec = runSignal sig cfg
        threshold = 0.95 :: Double
        limitSample !x
            | abs x <= threshold = x
            | x > threshold      = threshold + (1.0 - threshold) * tanh ((x - threshold) / (1.0 - threshold))
            | otherwise          = -limitSample (-x)
    in V.map (\(Frame l r) -> Frame (limitSample l) (limitSample r)) inVec
