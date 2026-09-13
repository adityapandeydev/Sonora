{-# LANGUAGE DeriveGeneric #-}
module Sonora.Music.Scale
    ( ScaleType (..)
    , Scale (..)
    , scaleIntervals
    , scaleNotes
    , scaleDegree
    -- Pre-built scale constructors
    , majorScale
    , minorScale
    , dorianScale
    , mixolydianScale
    , pentatonicMajor
    , pentatonicMinor
    , bluesScale
    , chromaticScale
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Interval
import Sonora.Music.Pitch (PitchClass)

-- | Named musical scale types and church modes.
data ScaleType
    = Major
    | NaturalMinor
    | HarmonicMinor
    | MelodicMinor
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Locrian
    | MajorPentatonic
    | MinorPentatonic
    | Blues
    | Chromatic
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

-- | A concrete scale with a root pitch and scale type.
data Scale = Scale
    { scaleRoot :: !PitchClass
    , scaleType :: !ScaleType
    } deriving (Eq, Show, Read, Generic)

-- | The list of cumulative intervals from the root for a given scale type.
scaleIntervals :: ScaleType -> [Interval]
scaleIntervals Major =
    [ unison, majorSecond, majorThird, perfectFourth, perfectFifth, majorSixth, majorSeventh ]
scaleIntervals NaturalMinor =
    [ unison, majorSecond, minorThird, perfectFourth, perfectFifth, minorSixth, minorSeventh ]
scaleIntervals HarmonicMinor =
    [ unison, majorSecond, minorThird, perfectFourth, perfectFifth, minorSixth, majorSeventh ]
scaleIntervals MelodicMinor =
    [ unison, majorSecond, minorThird, perfectFourth, perfectFifth, majorSixth, majorSeventh ]
scaleIntervals Dorian =
    [ unison, majorSecond, minorThird, perfectFourth, perfectFifth, majorSixth, minorSeventh ]
scaleIntervals Phrygian =
    [ unison, minorSecond, minorThird, perfectFourth, perfectFifth, minorSixth, minorSeventh ]
scaleIntervals Lydian =
    [ unison, majorSecond, majorThird, tritone, perfectFifth, majorSixth, majorSeventh ]
scaleIntervals Mixolydian =
    [ unison, majorSecond, majorThird, perfectFourth, perfectFifth, majorSixth, minorSeventh ]
scaleIntervals Locrian =
    [ unison, minorSecond, minorThird, perfectFourth, tritone, minorSixth, minorSeventh ]
scaleIntervals MajorPentatonic =
    [ unison, majorSecond, majorThird, perfectFifth, majorSixth ]
scaleIntervals MinorPentatonic =
    [ unison, minorThird, perfectFourth, perfectFifth, minorSeventh ]
scaleIntervals Blues =
    [ unison, minorThird, perfectFourth, tritone, perfectFifth, minorSeventh ]
scaleIntervals Chromatic =
    [ unison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth
    , tritone, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh ]

-- | Generate the list of pitch classes comprising a scale in one octave.
scaleNotes :: Scale -> [PitchClass]
scaleNotes (Scale root st) =
    map (`transposePitch` root) (scaleIntervals st)

-- | Get the n-th degree of a scale (1-indexed), extending into higher/lower octaves.
scaleDegree :: Scale -> Int -> PitchClass
scaleDegree (Scale root st) deg =
    let intervals = scaleIntervals st
        len       = length intervals
        zeroIdx   = deg - 1
        octaveOffset = zeroIdx `div` len
        modIdx       = zeroIdx `mod` len
        baseInterval = intervals !! modIdx
        totalInterval = baseInterval + Interval (octaveOffset * 12)
    in transposePitch totalInterval root

-- Scale constructors
majorScale :: PitchClass -> Scale
majorScale root = Scale root Major

minorScale :: PitchClass -> Scale
minorScale root = Scale root NaturalMinor

dorianScale :: PitchClass -> Scale
dorianScale root = Scale root Dorian

mixolydianScale :: PitchClass -> Scale
mixolydianScale root = Scale root Mixolydian

pentatonicMajor :: PitchClass -> Scale
pentatonicMajor root = Scale root MajorPentatonic

pentatonicMinor :: PitchClass -> Scale
pentatonicMinor root = Scale root MinorPentatonic

bluesScale :: PitchClass -> Scale
bluesScale root = Scale root Blues

chromaticScale :: PitchClass -> Scale
chromaticScale root = Scale root Chromatic
