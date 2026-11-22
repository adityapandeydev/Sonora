{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonora.Synth.Synthesizer
    ( renderNoteEvent
    , renderComposition
    , masterLimiter
    ) where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Sonora.Music.Composition
import Sonora.Music.Duration (BPM, durationToSeconds, durationToSamples)
import Sonora.Music.Interval (Interval (..), transposePitch)
import Sonora.Music.Note
import Sonora.Music.Pitch (pitchToFreq)
import Sonora.Signal.Core
import Sonora.Signal.Effects (chainEffects, pan)
import Sonora.Synth.Instrument
import Sonora.Synth.Voice

-- | Render a single timed note event to an audio Signal, preceded by silence
-- up to the note's start.
renderNoteEvent :: BPM -> Instrument -> TimedEvent Note -> Signal
renderNoteEvent bpm inst event = Signal $ \cfg ->
    let (startSamples, v) = renderNoteAt bpm inst event cfg
    in V.replicate startSamples mempty V.++ v

-- | The sample a note event starts at, and its samples from there on.
renderNoteAt :: BPM -> Instrument -> TimedEvent Note -> AudioConfig -> (Int, V.Vector Frame)
renderNoteAt defaultBpm defaultInst (TimedEvent start dur controls Note{..}) cfg =
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

    in (durationToSamples (configSampleRate cfg) activeBpm start, runSignal pannedSig cfg)

-- | Render an entire musical composition to a master Signal.
renderComposition :: BPM -> Instrument -> Music Note -> Signal
renderComposition bpm inst music =
    let events = linearize noteDuration music
        mixedNotes = Signal $ \cfg -> mixAt [ renderNoteAt bpm inst e cfg | e <- events ]
        -- Apply instrument-level master effect chain
        processed = chainEffects (instEffects inst) mixedNotes
    in masterLimiter processed

-- | Mix buffers that start at given samples. Equal to padding each with
-- leading silence and mixing them in order, but each buffer only costs its
-- own length and can be discarded once it has been added.
mixAt :: [(Int, V.Vector Frame)] -> V.Vector Frame
mixAt parts = runST (MV.new 0 >>= go 0 parts)
  where
    go :: Int -> [(Int, V.Vector Frame)] -> MV.MVector s Frame -> ST s (V.Vector Frame)
    go !used [] buf = V.unsafeFreeze (MV.take used buf)
    go !used ((start, v) : later) buf = do
        let !end = max used (start + V.length v)
        buf' <- ensure end buf
        add start v buf'
        go end later buf'

    -- Grow geometrically; frames never written to are silent.
    ensure :: Int -> MV.MVector s Frame -> ST s (MV.MVector s Frame)
    ensure needed buf
        | needed <= size = return buf
        | otherwise = do
            let extra = max (needed - size) size
            bigger <- MV.grow buf extra
            MV.set (MV.drop size bigger) mempty
            return bigger
      where size = MV.length buf

    add :: Int -> V.Vector Frame -> MV.MVector s Frame -> ST s ()
    add !start v buf = loop 0
      where
        !n = V.length v
        loop !i
            | i >= n = return ()
            | otherwise = do
                old <- MV.read buf (start + i)
                MV.write buf (start + i) (old <> V.unsafeIndex v i)
                loop (i + 1)

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
