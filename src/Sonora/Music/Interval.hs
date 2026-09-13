{-# LANGUAGE DeriveGeneric #-}
module Sonora.Music.Interval
    ( Interval (..)
    , transposePitch
    , intervalBetween
    -- Standard musical intervals
    , unison
    , minorSecond
    , majorSecond
    , minorThird
    , majorThird
    , perfectFourth
    , tritone
    , perfectFifth
    , minorSixth
    , majorSixth
    , minorSeventh
    , majorSeventh
    , octave
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Pitch (PitchClass, midiToPitch, pitchToMidi)

-- | An interval represented as a signed number of semitones.
newtype Interval = Interval { semitones :: Int }
    deriving (Eq, Ord, Num, Enum, Real, Integral, Show, Read, Generic)

-- Standard intervals in semitones
unison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth,
    tritone, perfectFifth, minorSixth, majorSixth, minorSeventh,
    majorSeventh, octave :: Interval

unison        = Interval 0
minorSecond   = Interval 1
majorSecond   = Interval 2
minorThird    = Interval 3
majorThird    = Interval 4
perfectFourth = Interval 5
tritone       = Interval 6
perfectFifth  = Interval 7
minorSixth    = Interval 8
majorSixth    = Interval 9
minorSeventh  = Interval 10
majorSeventh  = Interval 11
octave        = Interval 12

-- | Transpose a pitch class by a given interval.
transposePitch :: Interval -> PitchClass -> PitchClass
transposePitch (Interval st) pc = midiToPitch (pitchToMidi pc + st)

-- | Calculate the directed interval from the first pitch to the second.
intervalBetween :: PitchClass -> PitchClass -> Interval
intervalBetween fromPc toPc = Interval (pitchToMidi toPc - pitchToMidi fromPc)
