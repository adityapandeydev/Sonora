{-# LANGUAGE OverloadedStrings #-}
-- | Sonora project files: the whole project, as distinct from a .sonora
-- score, which holds only the score text.
--
-- A project file is UTF-8 JSON:
--
-- > { "format": "sonora-project", "version": 1,
-- >   "score": { "text": "...", "lastValid": "..." | null },
-- >   "tracks": [ { "id": "t1", "name": "lead", "patch": Patch | null, "mix": TrackMix } ],
-- >   "retired": [ { "id": "t4", "name": "pad", "patch": Patch | null, "mix": TrackMix } ],
-- >   "master": { "gain": 1 }, "nextTrack": 5,
-- >   "versions": [ { "label": "...", "text": "...", "settings": GenSettings | null } ] }
--
-- Reading checks everything a command would check (patch and mix ranges,
-- unique track ids, tracks matching the score) and rebuilds what can be
-- derived (diagnostics, instruments) from the score rather than trusting the
-- file. If the saved valid score no longer compiles, its tracks' settings
-- are kept as retired tracks instead of failing the whole file.
module Sonora.Project.Persistence
    ( ProjectFile (..)
    , SavedVersion (..)
    , LoadError (..)
    , loadErrorMessage
    , encodeProject
    , decodeProject
    , projectValue
    , decodeProjectValue
    , readProjectFile
    , writeAtomically
    , maxProjectBytes
    , projectExtension
    , scoreExtension
    ) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getFileSize, removeFile, renameFile)
import System.IO (IOMode (..), withBinaryFile)
import System.IO.Error (isAlreadyInUseError, isDoesNotExistError, isPermissionError)

import Sonora.DSL.AST (ASTSong (..), ASTTrack (..))
import Sonora.Generate.Score (GenSettings)
import Sonora.Project.Migration
import Sonora.Project.Score (ScoreAnalysis (..), analyzeScore)
import Sonora.Project.Types
import Sonora.Synth.Patch (Patch, ParamError (..), validatePatch)

projectExtension, scoreExtension :: String
projectExtension = ".sonoraproj"
scoreExtension = ".sonora"

-- | Larger files are refused before reading.
maxProjectBytes :: Integer
maxProjectBytes = 64 * 1024 * 1024

data SavedVersion = SavedVersion
    { savedLabel    :: !Text
    , savedText     :: !Text
    , savedSettings :: !(Maybe GenSettings)
    } deriving (Eq, Show)

data ProjectFile = ProjectFile
    { fileProject  :: !Project
    , fileVersions :: ![SavedVersion]  -- ^ Newest first.
    } deriving (Eq, Show)

data LoadError
    = Unreadable !Text      -- ^ The file could not be read.
    | TooLarge
    | NotJson !Text
    | NotAProject
    | NewerFormat !Int
    | UnknownFormat !Int
    | Damaged ![Text]       -- ^ Readable, but its contents are not acceptable.
    deriving (Eq, Show)

-- | What went wrong, in words, and that nothing was changed.
loadErrorMessage :: LoadError -> Text
loadErrorMessage e = case e of
    Unreadable why -> "The file could not be read: " <> why
    TooLarge -> "The file is too large to be a Sonora project."
    NotJson _ -> "This file is not a Sonora project."
    NotAProject -> "This file is not a Sonora project."
    NewerFormat v -> "This project was saved by a newer version of Sonora (format " <> showT v <> "). Update Sonora to open it."
    UnknownFormat v -> "This project uses an unknown format (" <> showT v <> ") and cannot be opened."
    Damaged (p : _) -> "This project file is damaged (" <> p <> ") and cannot be opened."
    Damaged [] -> "This project file is damaged and cannot be opened."

showT :: Show a => a -> Text
showT = T.pack . show

-------------------------------------------------------------------------------
-- Writing

encodeProject :: ProjectFile -> BL.ByteString
encodeProject = encode . projectValue

projectValue :: ProjectFile -> Value
projectValue (ProjectFile p versions) = object
    [ "format"    .= ("sonora-project" :: Text)
    , "version"   .= currentVersion
    , "score"     .= object
        [ "text" .= projectScoreText p
        , "lastValid" .= fmap validText (projectValidScore p) ]
    , "tracks"    .= [ trackJson (trackId t) (trackScoreName t) (trackPatchOverride t) (trackMix t) | t <- projectTracks p ]
    , "retired"   .= [ trackJson (retiredId r) (retiredName r) (retiredPatch r) (retiredMix r) | r <- projectRetired p ]
    , "master"    .= projectMaster p
    , "nextTrack" .= projectNextTrack p
    , "versions"  .= [ object ["label" .= l, "text" .= t, "settings" .= s] | SavedVersion l t s <- versions ]
    ]
  where
    trackJson i n patch mix = object ["id" .= i, "name" .= n, "patch" .= patch, "mix" .= mix]

-- | Write a file so that it is either complete or untouched: the bytes go to
-- a temporary file beside it, which then replaces it.
writeAtomically :: FilePath -> BL.ByteString -> IO ()
writeAtomically path bytes = do
    let part = path ++ ".part"
    result <- try $ do
        withBinaryFile part WriteMode (\h -> BL.hPut h bytes)
        renameFile part path
    case result of
        Right () -> return ()
        Left e -> do
            _ <- try (removeFile part) :: IO (Either IOException ())
            ioError e

-------------------------------------------------------------------------------
-- Reading

readProjectFile :: FilePath -> IO (Either LoadError ProjectFile)
readProjectFile path = do
    size <- try (getFileSize path)
    case size of
        Left e -> return (Left (Unreadable (ioMessage e)))
        Right n | n > maxProjectBytes -> return (Left TooLarge)
        Right _ -> do
            bytes <- try (BL.readFile path >>= \b -> BL.length b `seq` return b)
            return $ either (Left . Unreadable . ioMessage) decodeProject bytes

ioMessage :: IOException -> Text
ioMessage e
    | isDoesNotExistError e = "it no longer exists."
    | isPermissionError e = "Sonora is not allowed to read it."
    | isAlreadyInUseError e = "another program is using it."
    | otherwise = T.pack (show e)

decodeProject :: BL.ByteString -> Either LoadError ProjectFile
decodeProject bytes = either (Left . NotJson . T.pack) decodeProjectValue (eitherDecode bytes)

decodeProjectValue :: Value -> Either LoadError ProjectFile
decodeProjectValue v = do
    (format, version) <- either (const (Left NotAProject)) Right (parseEither header v)
    unless (format == ("sonora-project" :: Text)) (Left NotAProject)
    current <- case migrate version v of
        Left (NewerVersion n) -> Left (NewerFormat n)
        Left (NoMigration n) -> Left (UnknownFormat n)
        Left (MigrationFailed _ why) -> Left (Damaged [why])
        Right x -> Right x
    raw <- either (\m -> Left (Damaged [T.pack m])) Right (parseEither body current)
    build raw
  where
    header = withObject "project" $ \o -> (,) <$> o .: "format" <*> o .: "version"

data RawTrack = RawTrack !TrackId !Text !(Maybe Patch) !TrackMix

data Raw = Raw
    { rawText      :: !Text
    , rawLastValid :: !(Maybe Text)
    , rawTracks    :: ![RawTrack]
    , rawRetired   :: ![RawTrack]
    , rawMaster    :: !MasterMix
    , rawNext      :: !Int
    , rawVersions  :: ![SavedVersion]
    }

body :: Value -> Parser Raw
body = withObject "project" $ \o -> do
    score <- o .: "score"
    (txt, lastValid) <- withObject "score" (\s -> (,) <$> s .: "text" <*> s .:? "lastValid") score
    Raw txt lastValid
        <$> (o .:? "tracks" .!= [] >>= mapM track)
        <*> (o .:? "retired" .!= [] >>= mapM track)
        <*> o .:? "master" .!= defaultMasterMix
        <*> o .:? "nextTrack" .!= 1
        <*> (o .:? "versions" .!= [] >>= mapM version)
  where
    track = withObject "track" $ \t -> RawTrack <$> t .: "id" <*> t .: "name" <*> t .:? "patch" <*> t .:? "mix" .!= defaultTrackMix
    version = withObject "version" $ \x -> SavedVersion <$> x .: "label" <*> x .: "text" <*> x .:? "settings"

build :: Raw -> Either LoadError ProjectFile
build raw = do
    let all' = rawTracks raw ++ rawRetired raw
        ids = [ i | RawTrack i _ _ _ <- all' ]
        problems = concat
            [ [ "two tracks share an id" | length (nub ids) /= length ids ]
            , concat [ trackProblems i patch mix | RawTrack i _ patch mix <- all' ]
            , [ "master gain must be between 0 and 2" | not (inRange 0 2 (masterGain (rawMaster raw))) ]
            ]
    unless (null problems) (Left (Damaged problems))
    let analysed = analyzeScore (rawText raw)
        valid = do
            t <- rawLastValid raw
            ast <- analysisAst (analyzeScore t)
            return (ValidScore t ast)
        retiredOf (RawTrack i n p m) = RetiredTrack i n p m
    (tracks, retired) <- case valid of
        Just (ValidScore _ ast) -> do
            let names = map (T.pack . astTrackName) (astSongTracks ast)
            when (map (\(RawTrack _ n _ _) -> n) (rawTracks raw) /= names) $
                Left (Damaged ["its tracks do not match its score"])
            ts <- forM (zip (rawTracks raw) (astSongTracks ast)) $ \(RawTrack i n p m, a) ->
                return (TrackState i n (T.pack (astTrackInstrument a)) p m)
            return (ts, map retiredOf (rawRetired raw))
        -- The saved valid score no longer compiles: keep its tracks'
        -- settings for when they reappear.
        Nothing -> return ([], take maxRetiredTracks (map retiredOf (rawTracks raw ++ rawRetired raw)))
    let highest = maximum (0 : [ n | TrackId t <- ids, Just n <- [numbered t] ])
        project = Project
            { projectScoreText   = rawText raw
            , projectValidScore  = valid
            , projectDiagnostics = analysisDiagnostics analysed
            , projectTracks      = tracks
            , projectRetired     = retired
            , projectMaster      = rawMaster raw
            , projectNextTrack   = max (rawNext raw) (highest + 1)
            , projectRevision    = Revision 0
            }
    return (ProjectFile project (rawVersions raw))
  where
    numbered t = case T.uncons t of
        Just ('t', ds) | not (T.null ds), T.all isDigit ds -> Just (read (T.unpack ds) :: Int)
        _ -> Nothing
    trackProblems (TrackId i) patch (TrackMix g p _ _) = concat
        [ [ "track " <> i <> ": " <> paramPath e <> " " <> paramMessage e | Just pt <- [patch], e <- validatePatch pt ]
        , [ "track " <> i <> ": gain must be between 0 and 2" | not (inRange 0 2 g) ]
        , [ "track " <> i <> ": pan must be between -1 and 1" | not (inRange (-1) 1 p) ]
        , [ "a track has an empty id" | T.null i ]
        ]
    inRange lo hi x = not (isNaN x) && x >= lo && x <= hi
