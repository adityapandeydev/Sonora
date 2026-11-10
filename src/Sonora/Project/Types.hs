{-# LANGUAGE OverloadedStrings #-}
-- | The Sonora project document.
--
-- A project is a score (kept as the exact text the user wrote) plus
-- per-track sound and mix settings that live outside the score. Settings
-- are keyed by stable 'TrackId's, not by position, and project settings
-- take precedence over what the score declares.
--
-- Invariant: 'projectTracks' lists the tracks of 'projectValidScore' in
-- score order. When the score text does not compile, the last valid score
-- and its tracks stay in place, so the project remains playable.
module Sonora.Project.Types
    ( TrackId (..)
    , Revision (..)
    , nextRevision
    , TrackMix (..)
    , defaultTrackMix
    , MasterMix (..)
    , defaultMasterMix
    , Severity (..)
    , Diagnostic (..)
    , ValidScore (..)
    , TrackState (..)
    , hasCustomSettings
    , RetiredTrack (..)
    , maxRetiredTracks
    , Project (..)
    , scoreIsCurrent
    , findTrack
      -- * Commands
    , Command (..)
    , MixUpdate (..)
    , noMixUpdate
    , CommandError (..)
    ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Sonora.DSL.AST (ASTSong)
import Sonora.DSL.Parser (parseSonora)
import Sonora.Synth.Patch (ParamError, Patch)

-- | Stable identity of a track, independent of its name and position.
newtype TrackId = TrackId { trackIdText :: Text }
    deriving (Eq, Ord, Show)

instance ToJSON TrackId where
    toJSON = toJSON . trackIdText

instance FromJSON TrackId where
    parseJSON = fmap TrackId . parseJSON

-- | Increases with every accepted change to the project.
newtype Revision = Revision { revisionNumber :: Int }
    deriving (Eq, Ord, Show)

instance ToJSON Revision where
    toJSON = toJSON . revisionNumber

instance FromJSON Revision where
    parseJSON = fmap Revision . parseJSON

nextRevision :: Revision -> Revision
nextRevision (Revision n) = Revision (n + 1)

-- | Track-level mixer settings. Gain is linear (1 = unchanged); pan runs
-- from -1 (left) to 1 (right).
data TrackMix = TrackMix
    { mixGain :: !Double
    , mixPan  :: !Double
    , mixMute :: !Bool
    , mixSolo :: !Bool
    } deriving (Eq, Show)

defaultTrackMix :: TrackMix
defaultTrackMix = TrackMix 1 0 False False

instance ToJSON TrackMix where
    toJSON (TrackMix g p m s) = object ["gain" .= g, "pan" .= p, "mute" .= m, "solo" .= s]

instance FromJSON TrackMix where
    parseJSON = withObject "TrackMix" $ \o -> TrackMix
        <$> o .: "gain" <*> o .: "pan" <*> o .: "mute" <*> o .: "solo"

newtype MasterMix = MasterMix { masterGain :: Double }
    deriving (Eq, Show)

defaultMasterMix :: MasterMix
defaultMasterMix = MasterMix 1

instance ToJSON MasterMix where
    toJSON (MasterMix g) = object ["gain" .= g]

instance FromJSON MasterMix where
    parseJSON = withObject "MasterMix" $ \o -> MasterMix <$> o .: "gain"

data Severity = SeverityError | SeverityWarning
    deriving (Eq, Show)

-- | A problem found in the score. Positions are 1-based; semantic problems
-- that cannot be located precisely carry no position.
data Diagnostic = Diagnostic
    { diagSeverity :: !Severity
    , diagLine     :: !(Maybe Int)
    , diagColumn   :: !(Maybe Int)
    , diagMessage  :: !Text
    } deriving (Eq, Show)

instance ToJSON Diagnostic where
    toJSON (Diagnostic sev l c m) = object
        [ "severity" .= (case sev of SeverityError -> "error"; SeverityWarning -> "warning" :: Text)
        , "line" .= l, "column" .= c, "message" .= m ]

instance FromJSON Diagnostic where
    parseJSON = withObject "Diagnostic" $ \o -> do
        sev <- o .: "severity"
        s <- case sev :: Text of
            "error"   -> return SeverityError
            "warning" -> return SeverityWarning
            other     -> fail ("unknown severity " ++ show other)
        Diagnostic s <$> o .:? "line" <*> o .:? "column" <*> o .: "message"

-- | The most recent score text that compiled, with its syntax tree.
data ValidScore = ValidScore
    { validText :: !Text
    , validAst  :: !ASTSong
    } deriving (Eq, Show)

data TrackState = TrackState
    { trackId            :: !TrackId
    , trackScoreName          :: !Text          -- ^ Name in the score.
    , trackScoreInstrument    :: !Text          -- ^ Instrument the score assigns.
    , trackPatchOverride :: !(Maybe Patch) -- ^ Project patch; replaces the score's instrument.
    , trackMix           :: !TrackMix
    } deriving (Eq, Show)

-- | Whether a track carries settings beyond what the score implies.
hasCustomSettings :: TrackState -> Bool
hasCustomSettings t = trackPatchOverride t /= Nothing || trackMix t /= defaultTrackMix

-- | Settings of a track that disappeared from the score. A track that
-- reappears under the same name gets them back automatically; otherwise
-- they can be moved explicitly with 'AdoptRetiredSettings'.
data RetiredTrack = RetiredTrack
    { retiredId    :: !TrackId
    , retiredName  :: !Text
    , retiredPatch :: !(Maybe Patch)
    , retiredMix   :: !TrackMix
    } deriving (Eq, Show)

maxRetiredTracks :: Int
maxRetiredTracks = 32

data Project = Project
    { projectScoreText   :: !Text               -- ^ Exactly as edited.
    , projectValidScore  :: !(Maybe ValidScore) -- ^ Last score that compiled.
    , projectDiagnostics :: ![Diagnostic]       -- ^ For 'projectScoreText'.
    , projectTracks      :: ![TrackState]
    , projectRetired     :: ![RetiredTrack]     -- ^ Most recent first.
    , projectMaster      :: !MasterMix
    , projectNextTrack   :: !Int                -- ^ Next number for track ids.
    , projectRevision    :: !Revision
    } deriving (Eq, Show)

-- | Whether the playable score is the one currently in the editor.
scoreIsCurrent :: Project -> Bool
scoreIsCurrent p = fmap validText (projectValidScore p) == Just (projectScoreText p)

findTrack :: TrackId -> Project -> Maybe TrackState
findTrack tid = foldr (\t r -> if trackId t == tid then Just t else r) Nothing . projectTracks

-------------------------------------------------------------------------------
-- Commands

-- | Partial update of a track's mix; absent fields are left unchanged.
data MixUpdate = MixUpdate
    { updateGain :: !(Maybe Double)
    , updatePan  :: !(Maybe Double)
    , updateMute :: !(Maybe Bool)
    , updateSolo :: !(Maybe Bool)
    } deriving (Eq, Show)

noMixUpdate :: MixUpdate
noMixUpdate = MixUpdate Nothing Nothing Nothing Nothing

data Command
    = SetScoreText !Text
    | SetTrackPatch !TrackId !Patch
    | ResetTrackPatch !TrackId            -- ^ Return to the score's instrument.
    | UpdateTrackMix !TrackId !MixUpdate
    | SetMasterGain !Double
    | AdoptRetiredSettings !TrackId !TrackId  -- ^ From a retired track to a current one.
    deriving (Eq, Show)

data CommandError = CommandError
    { commandErrorCode    :: !Text          -- ^ @unknown_track@ or @invalid_value@.
    , commandErrorMessage :: !Text
    , commandErrorParams  :: ![ParamError]  -- ^ Offending fields, for @invalid_value@.
    } deriving (Eq, Show)

instance ToJSON CommandError where
    toJSON (CommandError c m ps) = object ["code" .= c, "message" .= m, "params" .= ps]

-- | Wire form: an object with a @"type"@ field.
--
-- > {"type": "setScoreText", "text": "..."}
-- > {"type": "setTrackPatch", "track": "t1", "patch": {...}}
-- > {"type": "resetTrackPatch", "track": "t1"}
-- > {"type": "updateTrackMix", "track": "t1", "gain": 0.8, "mute": true}
-- > {"type": "setMasterGain", "gain": 0.9}
-- > {"type": "adoptRetiredSettings", "from": "t3", "to": "t5"}
instance FromJSON Command where
    parseJSON = withObject "Command" $ \o -> do
        t <- o .: "type"
        case t :: Text of
            "setScoreText"         -> SetScoreText <$> o .: "text"
            "setTrackPatch"        -> SetTrackPatch <$> o .: "track" <*> o .: "patch"
            "resetTrackPatch"      -> ResetTrackPatch <$> o .: "track"
            "updateTrackMix"       -> UpdateTrackMix <$> o .: "track" <*> (MixUpdate
                <$> o .:? "gain" <*> o .:? "pan" <*> o .:? "mute" <*> o .:? "solo")
            "setMasterGain"        -> SetMasterGain <$> o .: "gain"
            "adoptRetiredSettings" -> AdoptRetiredSettings <$> o .: "from" <*> o .: "to"
            other                  -> fail ("unknown command type " ++ show other)

instance ToJSON Command where
    toJSON c = object $ case c of
        SetScoreText txt -> ["type" .= ("setScoreText" :: Text), "text" .= txt]
        SetTrackPatch tid p -> ["type" .= ("setTrackPatch" :: Text), "track" .= tid, "patch" .= p]
        ResetTrackPatch tid -> ["type" .= ("resetTrackPatch" :: Text), "track" .= tid]
        UpdateTrackMix tid (MixUpdate g pn m s) ->
            ["type" .= ("updateTrackMix" :: Text), "track" .= tid]
                ++ maybe [] (\v -> ["gain" .= v]) g ++ maybe [] (\v -> ["pan" .= v]) pn
                ++ maybe [] (\v -> ["mute" .= v]) m ++ maybe [] (\v -> ["solo" .= v]) s
        SetMasterGain g -> ["type" .= ("setMasterGain" :: Text), "gain" .= g]
        AdoptRetiredSettings from to -> ["type" .= ("adoptRetiredSettings" :: Text), "from" .= from, "to" .= to]

-------------------------------------------------------------------------------
-- Project JSON
--
-- The syntax tree is not stored: the valid score is kept as text and parsed
-- again on load. Diagnostics are stored as they were computed.

instance ToJSON TrackState where
    toJSON (TrackState tid name inst p m) = object
        ["id" .= tid, "name" .= name, "instrument" .= inst, "patchOverride" .= p, "mix" .= m]

instance FromJSON TrackState where
    parseJSON = withObject "Track" $ \o -> TrackState
        <$> o .: "id" <*> o .: "name" <*> o .: "instrument" <*> o .:? "patchOverride" <*> o .: "mix"

instance ToJSON RetiredTrack where
    toJSON (RetiredTrack tid name p m) = object ["id" .= tid, "name" .= name, "patch" .= p, "mix" .= m]

instance FromJSON RetiredTrack where
    parseJSON = withObject "RetiredTrack" $ \o -> RetiredTrack
        <$> o .: "id" <*> o .: "name" <*> o .:? "patch" <*> o .: "mix"

instance ToJSON Project where
    toJSON p = object
        [ "scoreText"       .= projectScoreText p
        , "validScoreText"  .= fmap validText (projectValidScore p)
        , "diagnostics"     .= projectDiagnostics p
        , "tracks"          .= projectTracks p
        , "retiredTracks"   .= projectRetired p
        , "master"          .= projectMaster p
        , "nextTrackNumber" .= projectNextTrack p
        , "revision"        .= projectRevision p
        ]

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o -> do
        validTxt <- o .:? "validScoreText"
        valid <- case validTxt of
            Nothing -> return Nothing
            Just txt -> case parseSonora "project" (T.unpack txt) of
                Right ast -> return (Just (ValidScore txt ast))
                Left err  -> fail ("stored valid score no longer parses: " ++ show err)
        Project
            <$> o .: "scoreText"
            <*> pure valid
            <*> o .: "diagnostics"
            <*> o .: "tracks"
            <*> o .: "retiredTracks"
            <*> o .: "master"
            <*> o .: "nextTrackNumber"
            <*> o .: "revision"
