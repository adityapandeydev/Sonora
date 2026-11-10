{-# LANGUAGE OverloadedStrings #-}
-- | Score analysis and track reconciliation.
--
-- 'analyzeScore' turns score text into either a usable syntax tree or
-- errors, plus warnings, without changing the text. 'reconcileTracks'
-- carries track identities and settings across score edits:
--
-- * A track keeps its id and settings when a track with the same name
--   exists before and after the edit. Track names must be unique, so this
--   match is never ambiguous.
-- * A track that disappears is retired with its settings. If a track with
--   that name comes back later, it gets the retired id and settings back.
-- * Anything else is a new track with a new id and default settings.
--   Renames are not guessed; settings can be moved explicitly from a
--   retired track instead, so a patch is never silently attached to a
--   different track.
module Sonora.Project.Score
    ( ScoreAnalysis (..)
    , analyzeScore
    , reconcileTracks
    , scorePatches
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Error (errorMessages, errorPos, showErrorMessages)
import Text.Parsec.Pos (sourceColumn, sourceLine)

import Sonora.DSL.AST
import Sonora.DSL.Parser (parseSonora)
import Sonora.Project.Types
import Sonora.Synth.Patch

data ScoreAnalysis = ScoreAnalysis
    { analysisAst         :: !(Maybe ASTSong)  -- ^ Present when there are no errors.
    , analysisDiagnostics :: ![Diagnostic]
    } deriving (Eq, Show)

-- | Patches declared by @instrument@ statements, by name.
scorePatches :: ASTSong -> Map.Map Text Patch
scorePatches ast = Map.fromList
    [ (T.pack (astInstName d), patchFromInstrumentDef d) | d <- astSongInstruments ast ]

analyzeScore :: Text -> ScoreAnalysis
analyzeScore txt = case parseSonora "score" src of
    Left err ->
        let pos = errorPos err
            msg = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
                    (errorMessages err)
        in ScoreAnalysis Nothing
            [Diagnostic SeverityError (Just (sourceLine pos)) (Just (sourceColumn pos)) (T.strip (T.pack msg))]
    Right ast ->
        let errors = duplicateTracks ast
            diags = errors ++ unknownInstruments ast ++ instrumentWarnings ast
        in ScoreAnalysis (if null errors then Just ast else Nothing) diags
  where
    src = T.unpack txt
    srcLines = lines src

    -- 1-based line of the n-th (0-based) "track <name>" header.
    trackLine name n =
        case drop n [ i | (i, l) <- zip [1 ..] srcLines, isTrackHeader name l ] of
            (i : _) -> Just i
            []      -> Nothing
    isTrackHeader name l =
        let rest = dropWhile isSpace l
        in ("track" `isPrefixOf` rest)
            && let afterKw = dropWhile isSpace (drop 5 rest)
               in name `isPrefixOf` afterKw
                    && case drop (length name) afterKw of
                        (c : _) -> not (isNameChar c)
                        []      -> True
    isNameChar c = c == '_' || c == '-' || c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c `elem` ['0' .. '9']

    duplicateTracks ast =
        let step (seen, counts, acc) t =
                let name = astTrackName t
                    n = Map.findWithDefault 0 name counts
                    acc' = if Set.member name seen
                        then Diagnostic SeverityError (trackLine name n) Nothing
                                ("Track name '" <> T.pack name <> "' is used more than once; track names must be unique")
                             : acc
                        else acc
                in (Set.insert name seen, Map.insert name (n + 1) counts, acc')
            (_, _, ds) = foldl' step (Set.empty, Map.empty :: Map.Map String Int, []) (astSongTracks ast)
        in reverse ds

    unknownInstruments ast =
        let custom = scorePatches ast
            known name = Map.member name custom || Map.member name presetPatches
        in [ Diagnostic SeverityWarning (trackLine (astTrackName t) 0) Nothing
                ("Track '" <> T.pack (astTrackName t) <> "' uses unknown instrument '" <> inst
                    <> "'; " <> patchName defaultPatch <> " is used instead")
           | t <- astSongTracks ast
           , let inst = T.pack (astTrackInstrument t)
           , not (known inst) ]

    instrumentWarnings ast =
        [ Diagnostic SeverityWarning Nothing Nothing
            ("Instrument '" <> T.pack (astInstName d) <> "': " <> paramPath e <> " " <> paramMessage e)
        | d <- astSongInstruments ast
        , e <- validatePatch (patchFromInstrumentDef d) ]

-- | Assign track states to the tracks of a new valid score. Returns the new
-- tracks (in score order), the retired list, and the next track number.
reconcileTracks :: Project -> ASTSong -> ([TrackState], [RetiredTrack], Int)
reconcileTracks old ast =
    let oldTracks = Map.fromList [ (trackScoreName t, t) | t <- projectTracks old ]
        newNames = Set.fromList [ T.pack (astTrackName t) | t <- astSongTracks ast ]

        step (next, retired, acc) astT =
            let name = T.pack (astTrackName astT)
                inst = T.pack (astTrackInstrument astT)
            in case Map.lookup name oldTracks of
                Just t -> (next, retired, t { trackScoreInstrument = inst } : acc)
                Nothing -> case break ((== name) . retiredName) retired of
                    (before, r : after) ->
                        ( next, before ++ after
                        , TrackState (retiredId r) name inst (retiredPatch r) (retiredMix r) : acc )
                    _ ->
                        ( next + 1, retired
                        , TrackState (TrackId ("t" <> T.pack (show next))) name inst Nothing defaultTrackMix : acc )

        (next', remainingRetired, revAcc) =
            foldl' step (projectNextTrack old, projectRetired old, []) (astSongTracks ast)

        newlyRetired =
            [ RetiredTrack (trackId t) (trackScoreName t) (trackPatchOverride t) (trackMix t)
            | t <- projectTracks old, not (Set.member (trackScoreName t) newNames) ]
    in (reverse revAcc, take maxRetiredTracks (newlyRetired ++ remainingRetired), next')
