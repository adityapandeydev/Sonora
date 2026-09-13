{-# LANGUAGE DeriveGeneric #-}
module Sonora.Music.Pitch
    ( Pitch (..)
    , Octave (..)
    , PitchClass (..)
    , MidiPitch
    , pitchToMidi
    , midiToPitch
    , midiToFreq
    , pitchToFreq
    -- Standard pitch aliases
    , c, cs, df, d, ds, ef, e, f, fs, gf, g, gs, af, a, as, bf, b
    -- Standard notes with octaves
    , c0, d0, e0, f0, g0, a0, b0
    , c1, d1, e1, f1, g1, a1, b1
    , c2, d2, e2, f2, g2, a2, b2
    , c3, d3, e3, f3, g3, a3, b3
    , c4, d4, e4, f4, g4, a4, b4
    , c5, d5, e5, f5, g5, a5, b5
    , c6, d6, e6, f6, g6, a6, b6
    , c7, d7, e7, f7, g7, a7, b7
    , c8, d8, e8, f8, g8, a8, b8
    ) where

import GHC.Generics (Generic)

-- | Chromatic pitch classes within an octave.
data Pitch
    = C
    | Cs
    | D
    | Ds
    | E
    | F
    | Fs
    | G
    | Gs
    | A
    | As
    | B
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Enharmonic aliases
c, cs, df, d, ds, ef, e, f, fs, gf, g, gs, af, a, as, bf, b :: Pitch
c  = C
cs = Cs
df = Cs
d  = D
ds = Ds
ef = Ds
e  = E
f  = F
fs = Fs
gf = Fs
g  = G
gs = Gs
af = Gs
a  = A
as = As
bf = As
b  = B

-- | Octave number (e.g. 4 for middle C).
newtype Octave = Octave Int
    deriving (Eq, Ord, Num, Enum, Real, Integral, Show, Read, Generic)

-- | A pitch qualified with an octave.
data PitchClass = PitchClass
    { pitchName   :: !Pitch
    , pitchOctave :: !Octave
    } deriving (Eq, Ord, Generic)

instance Show PitchClass where
    show (PitchClass p (Octave o)) = show p ++ show o

instance Read PitchClass where
    readsPrec p s =
        [ (PitchClass pitch (Octave oct), rest)
        | (pitch, rest1) <- readsPrec p s
        , (oct, rest)    <- readsPrec p rest1
        ]

-- | MIDI pitch number (0 to 127, where 60 is Middle C / C4).
type MidiPitch = Int

-- | Convert a 'PitchClass' to a standard MIDI pitch number.
pitchToMidi :: PitchClass -> MidiPitch
pitchToMidi (PitchClass p (Octave oct)) = (oct + 1) * 12 + fromEnum p

-- | Convert a standard MIDI pitch number to a 'PitchClass'.
midiToPitch :: MidiPitch -> PitchClass
midiToPitch midi =
    let oct = (midi `div` 12) - 1
        p   = toEnum (midi `mod` 12)
    in PitchClass p (Octave oct)

-- | Convert a MIDI pitch number to frequency in Hertz (A4 = 440 Hz).
midiToFreq :: MidiPitch -> Double
midiToFreq midi = 440.0 * (2.0 ** ((fromIntegral midi - 69.0) / 12.0))

-- | Convert a 'PitchClass' directly to frequency in Hertz.
pitchToFreq :: PitchClass -> Double
pitchToFreq = midiToFreq . pitchToMidi

-- Standard note definitions across octaves 0 to 8
c0, d0, e0, f0, g0, a0, b0 :: PitchClass
c0 = PitchClass C 0; d0 = PitchClass D 0; e0 = PitchClass E 0; f0 = PitchClass F 0; g0 = PitchClass G 0; a0 = PitchClass A 0; b0 = PitchClass B 0

c1, d1, e1, f1, g1, a1, b1 :: PitchClass
c1 = PitchClass C 1; d1 = PitchClass D 1; e1 = PitchClass E 1; f1 = PitchClass F 1; g1 = PitchClass G 1; a1 = PitchClass A 1; b1 = PitchClass B 1

c2, d2, e2, f2, g2, a2, b2 :: PitchClass
c2 = PitchClass C 2; d2 = PitchClass D 2; e2 = PitchClass E 2; f2 = PitchClass F 2; g2 = PitchClass G 2; a2 = PitchClass A 2; b2 = PitchClass B 2

c3, d3, e3, f3, g3, a3, b3 :: PitchClass
c3 = PitchClass C 3; d3 = PitchClass D 3; e3 = PitchClass E 3; f3 = PitchClass F 3; g3 = PitchClass G 3; a3 = PitchClass A 3; b3 = PitchClass B 3

c4, d4, e4, f4, g4, a4, b4 :: PitchClass
c4 = PitchClass C 4; d4 = PitchClass D 4; e4 = PitchClass E 4; f4 = PitchClass F 4; g4 = PitchClass G 4; a4 = PitchClass A 4; b4 = PitchClass B 4

c5, d5, e5, f5, g5, a5, b5 :: PitchClass
c5 = PitchClass C 5; d5 = PitchClass D 5; e5 = PitchClass E 5; f5 = PitchClass F 5; g5 = PitchClass G 5; a5 = PitchClass A 5; b5 = PitchClass B 5

c6, d6, e6, f6, g6, a6, b6 :: PitchClass
c6 = PitchClass C 6; d6 = PitchClass D 6; e6 = PitchClass E 6; f6 = PitchClass F 6; g6 = PitchClass G 6; a6 = PitchClass A 6; b6 = PitchClass B 6

c7, d7, e7, f7, g7, a7, b7 :: PitchClass
c7 = PitchClass C 7; d7 = PitchClass D 7; e7 = PitchClass E 7; f7 = PitchClass F 7; g7 = PitchClass G 7; a7 = PitchClass A 7; b7 = PitchClass B 7

c8, d8, e8, f8, g8, a8, b8 :: PitchClass
c8 = PitchClass C 8; d8 = PitchClass D 8; e8 = PitchClass E 8; f8 = PitchClass F 8; g8 = PitchClass G 8; a8 = PitchClass A 8; b8 = PitchClass B 8
