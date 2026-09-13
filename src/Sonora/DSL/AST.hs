{-# LANGUAGE DeriveGeneric #-}
module Sonora.DSL.AST
    ( ASTNote (..)
    , ASTItem (..)
    , ASTInstrumentDef (..)
    , ASTTrack (..)
    , ASTSong (..)
    ) where

import GHC.Generics (Generic)
import Sonora.Music.Chord (ChordQuality)
import Sonora.Music.Duration (BPM, Duration, TimeSignature)
import Sonora.Music.Note (Articulation, Velocity)
import Sonora.Music.Pitch (PitchClass)

-- | An individual note in the DSL.
data ASTNote = ASTNote
    { astPitch        :: !PitchClass
    , astDuration     :: !Duration
    , astVelocity     :: !(Maybe Velocity)
    , astArticulation :: !(Maybe Articulation)
    } deriving (Eq, Show, Generic)

-- | A musical event or structure in a track.
data ASTItem
    = ASTItemNote !ASTNote
    | ASTItemRest !Duration
    | ASTItemChord ![PitchClass] !Duration !(Maybe Velocity)
    | ASTItemNamedChord !PitchClass !ChordQuality !Duration
    | ASTItemRepeat !Int ![ASTItem]
    deriving (Eq, Show, Generic)

-- | Custom instrument declaration in the score.
data ASTInstrumentDef = ASTInstrumentDef
    { astInstName   :: !String
    , astInstOsc    :: !String                          -- "saw", "square", "sine", "triangle"
    , astInstCutoff :: !Double                          -- Hz
    , astInstRes    :: !Double                          -- Q
    , astInstADSR   :: !(Double, Double, Double, Double) -- A D S R
    } deriving (Eq, Show, Generic)

-- | A named track in the score.
data ASTTrack = ASTTrack
    { astTrackName       :: !String
    , astTrackInstrument :: !String
    , astTrackItems      :: ![ASTItem]
    } deriving (Eq, Show, Generic)

-- | The top-level composition document parsed from a .sonora file.
data ASTSong = ASTSong
    { astSongTitle       :: !String
    , astSongBPM         :: !(Maybe BPM)
    , astSongTimeSig     :: !(Maybe TimeSignature)
    , astSongInstruments :: ![ASTInstrumentDef]
    , astSongTracks      :: ![ASTTrack]
    } deriving (Eq, Show, Generic)
