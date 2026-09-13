{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sonora.Music.Note
    ( Velocity (..)
    , Articulation (..)
    , Note (..)
    , note
    , noteWithVel
    , noteWithArt
    -- Standard dynamic levels
    , ppp, pp, p, mp, mf, f, ff, fff
    -- Articulations
    , normal, staccato, legato, tenuto
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Duration (Duration, quarter)
import Sonora.Music.Pitch (PitchClass)

-- | Normalized velocity (loudness/dynamics) from 0.0 (silent) to 1.0 (full intensity).
newtype Velocity = Velocity { getVelocity :: Double }
    deriving (Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

instance Show Velocity where
    show (Velocity v)
        | v <= 0.20 = "ppp"
        | v <= 0.35 = "pp"
        | v <= 0.50 = "p"
        | v <= 0.65 = "mp"
        | v <= 0.75 = "mf"
        | v <= 0.85 = "f"
        | v <= 0.95 = "ff"
        | otherwise = "fff"

instance Read Velocity where
    readsPrec prec s =
        case lex s of
            [("ppp", rest)] -> [(ppp, rest)]
            [("pp", rest)]  -> [(pp, rest)]
            [("p", rest)]   -> [(p, rest)]
            [("mp", rest)]  -> [(mp, rest)]
            [("mf", rest)]  -> [(mf, rest)]
            [("f", rest)]   -> [(f, rest)]
            [("ff", rest)]  -> [(ff, rest)]
            [("fff", rest)] -> [(fff, rest)]
            _               -> [(Velocity v, rest) | (v, rest) <- readsPrec prec s]

-- Dynamic levels
ppp, pp, p, mp, mf, f, ff, fff :: Velocity
ppp = Velocity 0.15
pp  = Velocity 0.30
p   = Velocity 0.45
mp  = Velocity 0.60
mf  = Velocity 0.75
f   = Velocity 0.85
ff  = Velocity 0.95
fff = Velocity 1.00

-- | Note articulation affecting sustain length and attack character.
data Articulation
    = Normal
    | Staccato  -- Short, detached (typically ~50% gate duration)
    | Legato    -- Smooth, connected (typically 100% gate duration)
    | Tenuto    -- Held for full value with slight emphasis
    deriving (Eq, Show, Read, Generic)

normal, staccato, legato, tenuto :: Articulation
normal   = Normal
staccato = Staccato
legato   = Legato
tenuto   = Tenuto

-- | A concrete musical note with pitch, duration, velocity, and articulation.
data Note = Note
    { notePitch        :: !PitchClass
    , noteDuration     :: !Duration
    , noteVelocity     :: !Velocity
    , noteArticulation :: !Articulation
    } deriving (Eq, Show, Read, Generic)

-- | Construct a note with default mezzo-forte velocity and normal articulation.
note :: PitchClass -> Duration -> Note
note pc d = Note pc d mf Normal

-- | Construct a note with custom velocity.
noteWithVel :: PitchClass -> Duration -> Velocity -> Note
noteWithVel pc d v = Note pc d v Normal

-- | Construct a note with custom articulation.
noteWithArt :: PitchClass -> Duration -> Articulation -> Note
noteWithArt pc d a = Note pc d mf a
