{-# LANGUAGE OverloadedStrings #-}
-- | Turning a project into something playable.
--
-- Patch precedence for each track, highest first:
--
-- 1. the project's patch override for that track;
-- 2. an @instrument@ declared in the score with the track's instrument name;
-- 3. a built-in preset with that name;
-- 4. the default patch (reported as a warning by score analysis).
--
-- This matches the DSL evaluator for 2–4, so a project without overrides
-- sounds exactly like rendering its score directly.
module Sonora.Project.Compile
    ( PatchSource (..)
    , CompiledTrack (..)
    , CompiledProject (..)
    , compileProject
    , resolvePatch
    , compiledSong
    ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sonora.DSL.AST
import Sonora.DSL.Evaluator (evaluateItem)
import Sonora.Music.Composition (line)
import Sonora.Music.Duration (BPM, TimeSignature, defaultBPM, defaultTimeSignature)
import Sonora.Project.Score (scorePatches)
import Sonora.Project.Types
import Sonora.Render.Engine (Song (..), Track (..))
import Sonora.Synth.Patch

-- | Where a track's patch came from.
data PatchSource
    = FromOverride
    | FromScoreInstrument
    | FromPreset
    | FromDefault
    deriving (Eq, Show)

data CompiledTrack = CompiledTrack
    { compiledTrackId     :: !TrackId
    , compiledPatch       :: !Patch
    , compiledPatchSource :: !PatchSource
    , compiledMix         :: !TrackMix  -- ^ Carried for the mixer; not applied by 'compiledSong'.
    , compiledTrack       :: !Track
    }

data CompiledProject = CompiledProject
    { compiledRevision  :: !Revision
    , compiledIsCurrent :: !Bool  -- ^ False when the editor text has errors and an older score is used.
    , compiledTitle     :: !String
    , compiledBPM       :: !BPM
    , compiledTimeSig   :: !TimeSignature
    , compiledTracks    :: ![CompiledTrack]
    , compiledMaster    :: !MasterMix
    }

-- | The patch a track plays with, and where it came from.
resolvePatch :: Map.Map Text Patch -> TrackState -> (Patch, PatchSource)
resolvePatch declared t = case trackPatchOverride t of
    Just p -> (p, FromOverride)
    Nothing -> case Map.lookup (trackScoreInstrument t) declared of
        Just p -> (p, FromScoreInstrument)
        Nothing -> case lookupPresetPatch (trackScoreInstrument t) of
            Just p  -> (p, FromPreset)
            Nothing -> (defaultPatch, FromDefault)

-- | Compile the last valid score with the project's settings. Nothing when
-- no version of the score has ever compiled.
compileProject :: Project -> Maybe CompiledProject
compileProject p = do
    ValidScore _ ast <- projectValidScore p
    let declared = scorePatches ast
        tracks =
            [ CompiledTrack
                { compiledTrackId     = trackId ts
                , compiledPatch       = patch
                , compiledPatchSource = source
                , compiledMix         = trackMix ts
                , compiledTrack       = Track
                    { trackName       = T.unpack (trackScoreName ts)
                    , trackInstrument = compilePatch patch
                    , trackMusic      = line (map evaluateItem (astTrackItems astT))
                    }
                }
            | (ts, astT) <- zip (projectTracks p) (astSongTracks ast)
            , let (patch, source) = resolvePatch declared ts
            ]
    return CompiledProject
        { compiledRevision  = projectRevision p
        , compiledIsCurrent = scoreIsCurrent p
        , compiledTitle     = astSongTitle ast
        , compiledBPM       = maybe defaultBPM id (astSongBPM ast)
        , compiledTimeSig   = maybe defaultTimeSignature id (astSongTimeSig ast)
        , compiledTracks    = tracks
        , compiledMaster    = projectMaster p
        }

-- | The song to render. Track mix settings are not applied here.
compiledSong :: CompiledProject -> Song
compiledSong c = Song
    { songTitle   = compiledTitle c
    , songBPM     = compiledBPM c
    , songTimeSig = compiledTimeSig c
    , songTracks  = map compiledTrack (compiledTracks c)
    }
