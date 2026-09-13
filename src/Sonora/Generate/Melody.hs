{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Generate.Melody
    ( ArpMode (..)
    , generateMelody
    , arpeggiateWithMode
    , generateRhythm
    ) where

import GHC.Generics (Generic)
import Sonora.Generate.Random
import Sonora.Music.Chord (Chord, chordNotes)
import Sonora.Music.Composition
import Sonora.Music.Duration
import Sonora.Music.Note
import Sonora.Music.Scale

-- | Arpeggiator direction mode.
data ArpMode
    = ArpUp
    | ArpDown
    | ArpUpDown
    | ArpRandom
    deriving (Eq, Show, Read, Generic)

-- | Generate an arpeggiated sequence from a chord in a given mode.
arpeggiateWithMode :: ArpMode -> Seed -> Chord -> Duration -> Int -> Music Note
arpeggiateWithMode mode seed ch noteDur reps =
    let notes = chordNotes ch
        ordered = case mode of
            ArpUp     -> notes
            ArpDown   -> reverse notes
            ArpUpDown -> if length notes > 2
                         then notes ++ reverse (init (drop 1 notes))
                         else notes
            ArpRandom ->
                let prng = mkPRNG seed
                    pickN 0 _ _ = []
                    pickN n g list =
                        let (pc, g') = chooseRandom list g
                        in pc : pickN (n - 1) g' list
                in pickN (length notes * reps) prng notes

        repeatedPitches = take (reps * length notes) (cycle ordered)
    in line [ Prim (note pc noteDur) | pc <- repeatedPitches ]

-- | Generate a musically cohesive melody using Markov-inspired stepwise motion over a scale.
generateMelody :: Seed -> Scale -> Int -> [Duration] -> Int -> Music Note
generateMelody seed scl startDegree durPalette numNotes =
    let prng = mkPRNG seed
        go 0 _ _ = []
        go n !curDeg !g =
            let -- Step variations: 60% stepwise (-1, +1), 25% thirds (-2, +2), 15% fifths/unison
                (stepChoice, g1) = nextIntRange (1, 100) g
                delta = case () of
                    _ | stepChoice <= 35 -> 1
                      | stepChoice <= 70 -> -1
                      | stepChoice <= 82 -> 2
                      | stepChoice <= 94 -> -2
                      | otherwise        -> 0

                -- Keep melody within a pleasant range (scale degrees 1 to 15, i.e. 2 octaves)
                nextDeg = max 1 (min 15 (curDeg + delta))
                pc = scaleDegree scl nextDeg

                -- Pick duration from available palette
                (dur, g2) = chooseRandom durPalette g1

                -- Slight velocity dynamics variation
                (velOffset, g3) = nextDoubleRange (-0.08, 0.08) g2
                noteVel = Velocity (max 0.4 (min 0.95 (0.75 + velOffset)))

                nItem = Prim (Note pc dur noteVel Normal)
            in nItem : go (n - 1) nextDeg g3

    in line (go numNotes startDegree prng)

-- | Generate a random rhythmic pattern matching a target total duration.
generateRhythm :: Seed -> [Duration] -> Duration -> [Duration]
generateRhythm seed palette totalDur =
    let prng = mkPRNG seed
        go !accum !g
            | accum >= totalDur = []
            | otherwise =
                let remaining = totalDur - accum
                    validPal  = filter (<= remaining) palette
                    palToUse  = if null validPal then [remaining] else validPal
                    (d, g')   = chooseRandom palToUse g
                in d : go (accum + d) g'
    in go 0 prng
