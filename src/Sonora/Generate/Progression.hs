{-# LANGUAGE BangPatterns #-}
module Sonora.Generate.Progression
    ( generateProgression
    , voiceLead
    , progressionToMusic
    , commonProgressions
    ) where

import Sonora.Generate.Random
import Sonora.Music.Chord
import Sonora.Music.Composition
import Sonora.Music.Duration
import Sonora.Music.Note (Note)
import Sonora.Music.Pitch (pitchToMidi)
import Sonora.Music.Scale

-- | Standard chord progression templates in Roman numeral scale degrees.
commonProgressions :: [[Int]]
commonProgressions =
    [ [1, 5, 6, 4]      -- Axis of Awesome Pop Progression (I - V - vi - IV)
    , [6, 4, 1, 5]      -- Sensitive Female / Cinematic Progression (vi - IV - I - V)
    , [2, 5, 1, 1]      -- Jazz ii - V - I
    , [1, 4, 5, 4]      -- Classic Rock / Folk (I - IV - V - IV)
    , [1, 6, 4, 5]      -- 50s Doo-Wop Progression (I - vi - IV - V)
    , [1, 4, 1, 5]      -- Blues / Country Turnaround
    ]

-- | Build a diatonic triad on a given degree of a scale.
diatonicChordOnDegree :: Scale -> Int -> Chord
diatonicChordOnDegree scl deg =
    let root = scaleDegree scl deg
        third = scaleDegree scl (deg + 2)
        fifth = scaleDegree scl (deg + 4)
        -- Determine quality from semitone intervals
        interval3 = pitchToMidi third - pitchToMidi root
        interval5 = pitchToMidi fifth - pitchToMidi root
        quality = case (interval3 `mod` 12, interval5 `mod` 12) of
            (4, 7) -> MajorTriad
            (3, 7) -> MinorTriad
            (3, 6) -> DiminishedTriad
            (4, 8) -> AugmentedTriad
            _      -> MajorTriad
    in Chord root quality 0

-- | Pick a progression template and construct diatonic chords in the scale.
generateProgression :: Seed -> Scale -> Int -> [Chord]
generateProgression seed scl numChords =
    let prng = mkPRNG seed
        (chosenPattern, _) = chooseRandom commonProgressions prng
        extendedPattern = take numChords (cycle chosenPattern)
    in map (diatonicChordOnDegree scl) extendedPattern

-- | Smooth voice leading: chooses inversion for each chord to minimize total semitone movement from previous chord.
voiceLead :: [Chord] -> [Chord]
voiceLead [] = []
voiceLead (firstChord : restChords) = firstChord : go firstChord restChords
  where
    go _ [] = []
    go prev (curr : cs) =
        let possibleInversions = [ invertChord i curr | i <- [0, 1, 2] ]
            distance c1 c2 =
                let notes1 = chordNotes c1
                    notes2 = chordNotes c2
                in sum [ abs (pitchToMidi n2 - pitchToMidi n1) | (n1, n2) <- zip notes1 notes2 ]
            bestInversion = foldl1 (\best c -> if distance prev c < distance prev best then c else best) possibleInversions
        in bestInversion : go bestInversion cs

-- | Convert a progression of chords into a sequential musical phrase with equal durations per chord.
progressionToMusic :: [Chord] -> Duration -> Music Note
progressionToMusic chords chordDur =
    line [ chordToMusic ch chordDur | ch <- chords ]
