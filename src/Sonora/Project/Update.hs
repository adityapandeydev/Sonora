{-# LANGUAGE OverloadedStrings #-}
-- | Pure command handling for projects.
--
-- 'applyCommand' either rejects a command with a structured error and
-- leaves the project untouched, or returns the updated project. The
-- revision increases only when the project actually changes.
module Sonora.Project.Update
    ( emptyProject
    , newProject
    , applyCommand
    , applyCommands
    ) where

import Control.Monad (foldM, unless)
import Data.Text (Text)
import qualified Data.Text as T

import Sonora.Project.Score
import Sonora.Project.Types
import Sonora.Synth.Patch

-- | A project with an empty score.
emptyProject :: Project
emptyProject = newProject ""

-- | A project for the given score text, at revision 0.
newProject :: Text -> Project
newProject txt = setScore txt blank
  where
    blank = Project
        { projectScoreText   = ""
        , projectValidScore  = Nothing
        , projectDiagnostics = []
        , projectTracks      = []
        , projectRetired     = []
        , projectMaster      = defaultMasterMix
        , projectNextTrack   = 1
        , projectRevision    = Revision 0
        }

applyCommands :: [Command] -> Project -> Either CommandError Project
applyCommands cmds p = foldM (flip applyCommand) p cmds

applyCommand :: Command -> Project -> Either CommandError Project
applyCommand cmd p = do
    p' <- case cmd of
        SetScoreText txt -> Right (setScore txt p)

        SetTrackPatch tid patch -> do
            _ <- requireTrack tid p
            case validatePatch patch of
                [] -> Right (modifyTrack tid (\t -> t { trackPatchOverride = Just patch }) p)
                errs -> Left (CommandError "invalid_value" "Patch has values out of range" errs)

        ResetTrackPatch tid -> do
            _ <- requireTrack tid p
            Right (modifyTrack tid (\t -> t { trackPatchOverride = Nothing }) p)

        UpdateTrackMix tid (MixUpdate g pn m s) -> do
            _ <- requireTrack tid p
            let errs = maybe [] (checkRange "gain" 0 2) g ++ maybe [] (checkRange "pan" (-1) 1) pn
            unless (null errs) $ Left (CommandError "invalid_value" "Mix values out of range" errs)
            let apply mix = mix
                    { mixGain = maybe (mixGain mix) id g
                    , mixPan  = maybe (mixPan mix) id pn
                    , mixMute = maybe (mixMute mix) id m
                    , mixSolo = maybe (mixSolo mix) id s
                    }
            Right (modifyTrack tid (\t -> t { trackMix = apply (trackMix t) }) p)

        SetMasterGain g -> do
            let errs = checkRange "gain" 0 2 g
            unless (null errs) $ Left (CommandError "invalid_value" "Master gain out of range" errs)
            Right p { projectMaster = (projectMaster p) { masterGain = g } }

        AdoptRetiredSettings from to -> do
            _ <- requireTrack to p
            case break ((== from) . retiredId) (projectRetired p) of
                (before, r : after) -> Right $
                    modifyTrack to (\t -> t { trackPatchOverride = retiredPatch r, trackMix = retiredMix r })
                        p { projectRetired = before ++ after }
                _ -> Left (CommandError "unknown_track" ("No retired track " <> trackIdText from) [])
    return $ if p' == p then p else p' { projectRevision = nextRevision (projectRevision p) }

-- | Replace the score text. If it compiles, the valid score and tracks
-- follow it; otherwise only the text and diagnostics change.
setScore :: Text -> Project -> Project
setScore txt p =
    let ScoreAnalysis mAst diags = analyzeScore txt
    in case mAst of
        Nothing -> p { projectScoreText = txt, projectDiagnostics = diags }
        Just ast ->
            let (tracks, retired, next) = reconcileTracks p ast
            in p { projectScoreText   = txt
                 , projectValidScore  = Just (ValidScore txt ast)
                 , projectDiagnostics = diags
                 , projectTracks      = tracks
                 , projectRetired     = retired
                 , projectNextTrack   = next
                 }

requireTrack :: TrackId -> Project -> Either CommandError TrackState
requireTrack tid p = maybe (Left (CommandError "unknown_track" ("No track " <> trackIdText tid) [])) Right (findTrack tid p)

modifyTrack :: TrackId -> (TrackState -> TrackState) -> Project -> Project
modifyTrack tid f p = p { projectTracks = [ if trackId t == tid then f t else t | t <- projectTracks p ] }

checkRange :: Text -> Double -> Double -> Double -> [ParamError]
checkRange name lo hi x =
    [ ParamError name ("must be between " <> T.pack (show lo) <> " and " <> T.pack (show hi))
    | isNaN x || isInfinite x || x < lo || x > hi ]
