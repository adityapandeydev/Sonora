{-# LANGUAGE DeriveGeneric #-}
module Sonora.Music.Chord
    ( ChordQuality (..)
    , Chord (..)
    , chordIntervals
    , chordNotes
    , invertChord
    -- Chord constructors
    , majorChord
    , minorChord
    , dimChord
    , augChord
    , dom7Chord
    , maj7Chord
    , min7Chord
    , halfDim7Chord
    , dim7Chord
    , sus2Chord
    , sus4Chord
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Interval
import Sonora.Music.Pitch (PitchClass)

-- | Chord qualities including triads and 7ths.
data ChordQuality
    = MajorTriad
    | MinorTriad
    | DiminishedTriad
    | AugmentedTriad
    | Dominant7
    | Major7
    | Minor7
    | HalfDiminished7
    | Diminished7
    | Sus2
    | Sus4
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

-- | A chord with a root note, quality, and inversion (0 = root position, 1 = 1st inversion, etc.).
data Chord = Chord
    { chordRoot      :: !PitchClass
    , chordQuality   :: !ChordQuality
    , chordInversion :: !Int
    } deriving (Eq, Show, Read, Generic)

-- | The intervals from the root that define each chord quality.
chordIntervals :: ChordQuality -> [Interval]
chordIntervals MajorTriad      = [ unison, majorThird, perfectFifth ]
chordIntervals MinorTriad      = [ unison, minorThird, perfectFifth ]
chordIntervals DiminishedTriad = [ unison, minorThird, tritone ]
chordIntervals AugmentedTriad  = [ unison, majorThird, minorSixth ]
chordIntervals Dominant7       = [ unison, majorThird, perfectFifth, minorSeventh ]
chordIntervals Major7          = [ unison, majorThird, perfectFifth, majorSeventh ]
chordIntervals Minor7          = [ unison, minorThird, perfectFifth, minorSeventh ]
chordIntervals HalfDiminished7 = [ unison, minorThird, tritone, minorSeventh ]
chordIntervals Diminished7     = [ unison, minorThird, tritone, majorSixth ] -- 9 semitones = diminished 7th
chordIntervals Sus2            = [ unison, majorSecond, perfectFifth ]
chordIntervals Sus4            = [ unison, perfectFourth, perfectFifth ]

-- | Compute the pitches of a chord, taking inversion into account.
chordNotes :: Chord -> [PitchClass]
chordNotes (Chord root quality inv) =
    let baseNotes = map (`transposePitch` root) (chordIntervals quality)
        len       = length baseNotes
        effInv    = if len == 0 then 0 else inv `mod` len
        (low, hi) = splitAt effInv baseNotes
        inverted  = hi ++ map (transposePitch (Interval 12)) low
    in inverted

-- | Apply an inversion to a chord.
invertChord :: Int -> Chord -> Chord
invertChord n c = c { chordInversion = chordInversion c + n }

-- Convenient chord builders
majorChord, minorChord, dimChord, augChord, dom7Chord, maj7Chord,
    min7Chord, halfDim7Chord, dim7Chord, sus2Chord, sus4Chord :: PitchClass -> Chord

majorChord root = Chord root MajorTriad 0
minorChord root = Chord root MinorTriad 0
dimChord   root = Chord root DiminishedTriad 0
augChord   root = Chord root AugmentedTriad 0
dom7Chord  root = Chord root Dominant7 0
maj7Chord  root = Chord root Major7 0
min7Chord  root = Chord root Minor7 0
halfDim7Chord root = Chord root HalfDiminished7 0
dim7Chord  root = Chord root Diminished7 0
sus2Chord  root = Chord root Sus2 0
sus4Chord  root = Chord root Sus4 0
