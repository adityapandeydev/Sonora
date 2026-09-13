{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Music.Composition
    ( Control (..)
    , Music (..)
    , TimedEvent (..)
    -- Operators
    , (|:>)
    , (|||)
    -- Constructors and helpers
    , rest
    , singleNote
    , chordToMusic
    , arpeggiate
    , line
    , chordGroup
    , repeatM
    , transposeMusic
    , setInstrument
    , setTempo
    , setPan
    , scaleVolume
    -- Analysis & Linearization
    , musicDuration
    , linearize
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Chord (Chord, chordNotes)
import Sonora.Music.Duration (BPM, Duration, quarter, whole)
import Sonora.Music.Interval (Interval, transposePitch)
import Sonora.Music.Note (Note (..), note)
import Sonora.Music.Pitch (PitchClass)

-- | Modifiers that apply over a scope of musical composition.
data Control
    = SetTempo !BPM
    | Transpose !Interval
    | ScaleVolume !Double
    | SetInstrument !String
    | SetPan !Double
    deriving (Eq, Show, Read, Generic)

-- | The core algebraic music representation.
-- Music behaves as a free algebra supporting sequential and parallel composition.
data Music a
    = Prim !a
    | Rest !Duration
    | Sequential !(Music a) !(Music a)
    | Parallel !(Music a) !(Music a)
    | Modify !Control !(Music a)
    deriving (Eq, Show, Read, Functor, Generic)

-- | Sequential composition forms a Semigroup.
instance Semigroup (Music a) where
    (<>) = Sequential

-- | Monoid with identity as zero-duration rest.
instance Monoid (Music a) where
    mempty = Rest 0

-- | Sequential composition operator: played one after another.
(|:>) :: Music a -> Music a -> Music a
(|:>) = Sequential
infixr 6 |:>

-- | Parallel composition operator: played simultaneously.
(|||) :: Music a -> Music a -> Music a
(|||) = Parallel
infixr 5 |||

-- | A musical rest with specified duration.
rest :: Duration -> Music a
rest = Rest

-- | Embed a single note into music.
singleNote :: Note -> Music Note
singleNote = Prim

-- | Play all notes in a chord in parallel with the given duration.
chordToMusic :: Chord -> Duration -> Music Note
chordToMusic ch d =
    chordGroup [ Prim (note pc d) | pc <- chordNotes ch ]

-- | Arpeggiate notes of a chord sequentially with equal divisions of duration.
arpeggiate :: Chord -> Duration -> Music Note
arpeggiate ch totalDur =
    let notes = chordNotes ch
        count = length notes
    in if count == 0
       then Rest totalDur
       else let noteDur = totalDur / fromIntegral count
            in line [ Prim (note pc noteDur) | pc <- notes ]

-- | Fold a list of musical phrases sequentially.
line :: [Music a] -> Music a
line = mconcat

-- | Fold a list of musical phrases in parallel.
chordGroup :: [Music a] -> Music a
chordGroup []     = Rest 0
chordGroup (m:ms) = foldr (|||) m ms

-- | Repeat a musical phrase n times.
repeatM :: Int -> Music a -> Music a
repeatM n m
    | n <= 0    = Rest 0
    | otherwise = line (replicate n m)

-- | Transpose a musical phrase by an interval.
transposeMusic :: Interval -> Music Note -> Music Note
transposeMusic interval = Modify (Transpose interval)

-- | Assign an instrument to a musical phrase.
setInstrument :: String -> Music a -> Music a
setInstrument inst = Modify (SetInstrument inst)

-- | Set tempo for a musical phrase.
setTempo :: BPM -> Music a -> Music a
setTempo bpm = Modify (SetTempo bpm)

-- | Set stereo pan (-1.0 left, 0.0 center, +1.0 right).
setPan :: Double -> Music a -> Music a
setPan pan = Modify (SetPan pan)

-- | Scale volume / velocity of a musical phrase.
scaleVolume :: Double -> Music a -> Music a
scaleVolume vol = Modify (ScaleVolume vol)

-- | Compute the total musical duration of a composition.
musicDuration :: (a -> Duration) -> Music a -> Duration
musicDuration getDur = go
  where
    go (Prim a)         = getDur a
    go (Rest d)         = d
    go (Sequential a b) = go a + go b
    go (Parallel a b)   = max (go a) (go b)
    go (Modify _ a)     = go a

-- | An instantiated musical event in absolute musical time.
data TimedEvent a = TimedEvent
    { eventStart    :: !Duration
    , eventDuration :: !Duration
    , eventControls :: ![Control]
    , eventValue    :: !a
    } deriving (Eq, Show, Functor, Generic)

-- | Flatten a hierarchical 'Music' tree into a list of scheduled timed events.
linearize :: (a -> Duration) -> Music a -> [TimedEvent a]
linearize getDur root = go 0 [] root
  where
    go !start controls (Prim a) =
        [TimedEvent start (getDur a) controls a]

    go !_ _ (Rest _) =
        []

    go !start controls (Sequential a b) =
        let eventsA = go start controls a
            durA    = musicDuration getDur a
            eventsB = go (start + durA) controls b
        in eventsA ++ eventsB

    go !start controls (Parallel a b) =
        go start controls a ++ go start controls b

    go !start controls (Modify ctrl a) =
        go start (ctrl : controls) a
