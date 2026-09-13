{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sonora.Music.Duration
    ( Duration (..)
    , BPM (..)
    , TimeSignature (..)
    -- Note value constants
    , doubleWhole
    , whole
    , half
    , quarter
    , eighth
    , sixteenth
    , thirtySecond
    , sixtyFourth
    -- Modifiers
    , dotted
    , doubleDotted
    , dottedDouble
    , triplet
    , tuplet
    -- Time conversions
    , durationToSeconds
    , secondsToDuration
    , durationToSamples
    , defaultBPM
    , defaultTimeSignature
    ) where

import Data.Ratio ((%))
import GHC.Generics (Generic)

-- | Exact musical duration represented as a rational fraction of a whole note.
-- 1 = Whole note, 1/2 = Half note, 1/4 = Quarter note, etc.
newtype Duration = Duration { toRationalDuration :: Rational }
    deriving (Eq, Ord, Num, Fractional, Generic)

instance Real Duration where
    toRational (Duration r) = r

instance RealFrac Duration where
    properFraction (Duration r) =
        let (n, f) = properFraction r
        in (n, Duration f)

instance Show Duration where
    show (Duration r)
        | r == 1    = "whole"
        | r == 1%2  = "half"
        | r == 1%4  = "quarter"
        | r == 1%8  = "eighth"
        | r == 1%16 = "sixteenth"
        | r == 1%32 = "thirtySecond"
        | r == 3%8  = "dotted half"
        | r == 3%16 = "dotted quarter"
        | r == 3%32 = "dotted eighth"
        | otherwise = show (fromRational r :: Double)

instance Read Duration where
    readsPrec p s =
        case lex s of
            [("doubleWhole", rest)]  -> [(doubleWhole, rest)]
            [("whole", rest)]        -> [(whole, rest)]
            [("half", rest)]         -> [(half, rest)]
            [("quarter", rest)]      -> [(quarter, rest)]
            [("eighth", rest)]       -> [(eighth, rest)]
            [("sixteenth", rest)]    -> [(sixteenth, rest)]
            [("thirtySecond", rest)] -> [(thirtySecond, rest)]
            [("sixtyFourth", rest)]  -> [(sixtyFourth, rest)]
            _                        -> [(Duration (toRational (n :: Double)), rest) | (n, rest) <- readsPrec p s]

-- | Tempo in Beats Per Minute (where 1 beat = quarter note).
newtype BPM = BPM { getBPM :: Double }
    deriving (Eq, Ord, Num, Fractional, Show, Read, Generic)

-- | Musical time signature: numerator (beats per measure) and denominator (beat unit).
-- e.g. TimeSignature 4 4 = 4/4, TimeSignature 3 4 = 3/4, TimeSignature 6 8 = 6/8.
data TimeSignature = TimeSignature
    { tsNumerator   :: !Int
    , tsDenominator :: !Int
    } deriving (Eq, Show, Read, Generic)

defaultBPM :: BPM
defaultBPM = BPM 120.0

defaultTimeSignature :: TimeSignature
defaultTimeSignature = TimeSignature 4 4

-- Standard durations
doubleWhole, whole, half, quarter, eighth, sixteenth, thirtySecond, sixtyFourth :: Duration
doubleWhole  = Duration 2
whole        = Duration 1
half         = Duration (1 % 2)
quarter      = Duration (1 % 4)
eighth       = Duration (1 % 8)
sixteenth    = Duration (1 % 16)
thirtySecond = Duration (1 % 32)
sixtyFourth  = Duration (1 % 64)

-- | Dotted note: adds half its value (1.5x).
dotted :: Duration -> Duration
dotted (Duration d) = Duration (d * (3 % 2))

-- | Double dotted note: adds 3/4 its value (1.75x).
doubleDotted :: Duration -> Duration
doubleDotted (Duration d) = Duration (d * (7 % 4))

dottedDouble :: Duration -> Duration
dottedDouble = doubleDotted

-- | Triplet: fits 3 notes into the space of 2 (2/3 duration).
triplet :: Duration -> Duration
triplet (Duration d) = Duration (d * (2 % 3))

-- | General n:m tuplet: fits n notes into the space of m notes (m/n duration).
tuplet :: Integer -> Integer -> Duration -> Duration
tuplet n m (Duration d) = Duration (d * (m % n))

-- | Convert a musical duration to audio seconds at a given BPM.
-- At 120 BPM, 1 beat (quarter note = 1/4) is 0.5s.
-- Therefore, whole note (1) is 4 * (60 / BPM) = 240 / BPM seconds.
durationToSeconds :: BPM -> Duration -> Double
durationToSeconds (BPM bpm) (Duration d) =
    fromRational d * (240.0 / bpm)

-- | Convert seconds back to musical duration at a given BPM.
secondsToDuration :: BPM -> Double -> Duration
secondsToDuration (BPM bpm) secs =
    Duration (toRational (secs * (bpm / 240.0)))

-- | Convert a musical duration to sample frames at given sample rate and BPM.
durationToSamples :: Int -> BPM -> Duration -> Int
durationToSamples sampleRate bpm dur =
    round (durationToSeconds bpm dur * fromIntegral sampleRate)
