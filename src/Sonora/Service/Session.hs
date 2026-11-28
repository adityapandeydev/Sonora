{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Application state served by @sonora-service@: the open project, its
-- render jobs, and song playback.
--
-- Rendering: every accepted change to what the song would sound like
-- starts a render on a worker thread. At most one render runs; starting a
-- new one cancels the old one (reported as @render.cancelled@), and a
-- result is only accepted from the job that is still current, so a stale
-- render can never replace a newer one. Jobs are identified by a job id
-- and the project revision they were started from.
--
-- A render is /current/ when the project inputs it was made from (valid
-- score, track settings, master settings) equal the project's inputs now.
-- Editing the score into an invalid state does not make the render
-- outdated: the playable score has not changed, which is reported
-- separately as @scoreIsCurrent@.
--
-- Playback always uses the latest finished render. When a newer render
-- finishes it is swapped in at the same position, keeping the loop.
--
-- Transport positions are what is being heard, not what has been queued to
-- the device. Position, loop, and volume belong to the session rather than
-- the device, so they survive losing and reopening it. Volume is a listening
-- level and does not affect renders.
module Sonora.Service.Session
    ( SessionConfig (..)
    , defaultSessionConfig
    , starterScore
    , Session
    , Emit
    , newSession
    , closeSession
      -- * Operations (results are protocol JSON)
    , projectView
    , setScore
    , applyProjectCommand
    , startRender
    , cancelRender
    , play
    , pause
    , stop
    , seek
    , setLoop
    , setVolume
    , transportView
    , renderPeaks
    , scoreTiming
    , generate
    , restoreVersion
    , startAudition
    , updateAudition
    , releaseAudition
    , stopAudition
    , ExportRequest (..)
    , startExport
    , cancelExport
    , exportRates
    , saveProject
    , openProject
    , startNewProject
    , importScore
    , exportScore
    , undoChange
    , redoChange
    , checkRecovery
    , restoreRecovery
    , discardRecovery
    , autosaveNow
    , maxVersions
    , maxPeakBuckets
    , minLoopSeconds
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, forever, void, when)
import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)

import Sonora.Audio.Audition
import Sonora.Audio.Backend
import Sonora.Audio.Device (openDeviceBackend)
import Sonora.DSL.Print (printSong)
import Sonora.DSL.AST (ASTSong (..))
import Sonora.Generate.Score
import Sonora.Project.Compile
import Sonora.Project.Layout (outline, songInfo, timing)
import Sonora.Project.Render (Stems, emptyStems, renderProject)
import Sonora.Project.Persistence
import Sonora.Project.History
import Sonora.Project.Recovery
import qualified Data.Text.Encoding as TE
import System.FilePath (takeFileName)
import System.Directory (getFileSize)
import Sonora.Render.WAV (BitDepth (..), encodeWAV)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesDirectoryExist, removeFile, renameFile)
import System.FilePath (takeDirectory, takeExtension)
import System.IO.Error (ioeGetErrorType, isAlreadyInUseError, isDoesNotExistError, isFullError, isPermissionError)
import GHC.IO.Exception (IOErrorType (InappropriateType))
import Sonora.Project.Score (scorePatches)
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Service.Protocol
import Sonora.Signal.Core (AudioConfig (..), Frame, defaultConfig)
import Sonora.Signal.Waveform (waveformPeaks)
import Sonora.Synth.Patch (ParamError (..), Patch, validatePatch)

type Emit = Event -> IO ()

data SessionConfig = SessionConfig
    { sessionSampleRate   :: !Int
      -- | Renders a compiled project, reporting @(completed, total)@ progress.
    , sessionRenderer     :: Int -> (Int -> Int -> IO ()) -> Stems -> CompiledProject -> IO (Stems, V.Vector Frame)
    , sessionOpenAudio    :: Int -> IO (Either AudioError AudioBackend)
    , sessionInitialScore :: !Text
      -- | Interval between @transport@ events while playing, in microseconds.
    , sessionTelemetryMicros :: !Int
      -- | Renders one auditioned note.
    , sessionAuditionRenderer :: Int -> Patch -> AuditionNote -> IO (V.Vector Frame)
      -- | Where unsaved work is kept between saves; no autosave without one.
    , sessionRecoveryDir :: !(Maybe FilePath)
      -- | How often unsaved work is written, in microseconds.
    , sessionAutosaveMicros :: !Int
    }

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
    { sessionSampleRate      = 44100
    , sessionRenderer        = renderProject
    , sessionOpenAudio       = openDeviceBackend
    , sessionInitialScore    = starterScore
    , sessionTelemetryMicros = 40000
    , sessionAuditionRenderer = \sr p n -> evaluate (renderAudition sr p n)
    , sessionRecoveryDir = Nothing
    , sessionAutosaveMicros = 2000000
    }

-- | The score a new session starts with.
starterScore :: Text
starterScore = T.unlines
    [ "title \"Untitled\""
    , "tempo 96"
    , "time 4/4"
    , ""
    , "track keys (piano):"
    , "    chord(C4, E4, G4) half"
    , "    chord(A3, C4, E4) half"
    , "    chord(F3, A3, C4) half"
    , "    chord(G3, B3, D4) half"
    , ""
    , "track bass (bass):"
    , "    C2 half"
    , "    A1 half"
    , "    F1 half"
    , "    G1 half"
    ]

-- | What a render depends on.
data RenderInputs = RenderInputs !(Maybe Text) ![TrackState] !MasterMix
    deriving Eq

renderInputs :: Project -> RenderInputs
renderInputs p = RenderInputs (validText <$> projectValidScore p) (projectTracks p) (projectMaster p)

data Job = Job
    { jobId       :: !Int
    , jobRevision :: !Revision
    , jobInputs   :: !RenderInputs
    , jobThread   :: !ThreadId
    }

data Finished = Finished
    { finJobId    :: !Int
    , finRevision :: !Revision
    , finInputs   :: !RenderInputs
    , finFrames   :: !(V.Vector Frame)
    , finRate     :: !Int
    , finMillis   :: !Int
    }

data Failure = Failure
    { failJobId    :: !Int
    , failRevision :: !Revision
    , failInputs   :: !RenderInputs
    , failMessage  :: !Text
    }

data St = St
    { stProject  :: !Project
    , stNextJob  :: !Int
    , stRunning  :: !(Maybe Job)
    , stFinished :: !(Maybe Finished)
    , stFailure  :: !(Maybe Failure)       -- ^ The latest job failed.
    , stAudio    :: !(Maybe AudioBackend)  -- ^ Opened on first play.
    , stLoaded   :: !(Maybe Finished)      -- ^ Render loaded on the song voice.
    , stWatcher  :: !(Maybe ThreadId)      -- ^ Emits transport events while playing.
    , stCue      :: !Double                -- ^ Position in seconds while nothing is loaded.
    , stLoop     :: !(Maybe (Double, Double)) -- ^ Loop range in seconds.
    , stVolume   :: !Double
    , stLost     :: !Bool                  -- ^ The device went away; cleared by reopening it.
    , stValidSince :: !Revision            -- ^ Revision at which the valid score last changed.
    , stVersions :: ![ScoreVersion]        -- ^ Newest first, at most 'maxVersions'.
    , stNextVersion :: !Int
    , stAudition :: !(Maybe Audition)      -- ^ The note being auditioned.
    , stNextAudition :: !Int
    , stStems :: !Stems                    -- ^ Rendered tracks, reused while they apply.
    , stExport :: !(Maybe ExportJob)
    , stNextExport :: !Int
    , stFile :: !(Maybe FilePath)          -- ^ Where the project was opened from or saved to.
    , stSavedRevision :: !Revision         -- ^ Revision with nothing unsaved.
    , stHistory :: !History
    , stAutosaved :: !(Maybe Revision)     -- ^ Revision in the autosave, if one is written.
    , stRecoveryPending :: !Bool           -- ^ An autosave from before awaits a decision.
    , stAutosaver :: !(Maybe ThreadId)
    }

data ExportJob = ExportJob
    { exportId     :: !Int
    , exportThread :: !ThreadId
    , exportPart   :: !FilePath  -- ^ Written first, renamed when complete.
    }

data ExportRequest = ExportRequest
    { exportPath     :: !FilePath
    , exportRate     :: !Int
    , exportBitDepth :: !Int  -- ^ 16, 24, or 32 (float).
    }

-- | One auditioned note. Each change (a new patch, the release) renders it
-- again as a new revision; only the latest revision reaches the speaker.
data Audition = Audition
    { auId       :: !Int
    , auPatch    :: !Patch
    , auNote     :: !AuditionNote
    , auRevision :: !Int               -- ^ Latest render requested.
    , auShown    :: !Int               -- ^ Revision on the audition voice; 0 before the first.
    , auReleased :: !Bool
    , auWorker   :: !(Maybe ThreadId)
    }

-- | A score kept so that generating or restoring never loses the text it replaces.
data ScoreVersion = ScoreVersion
    { versionId       :: !Int
    , versionLabel    :: !Text
    , versionText     :: !Text
    , versionRevision :: !Revision           -- ^ Project revision when it was kept.
    , versionSettings :: !(Maybe GenSettings) -- ^ Present for generated scores.
    }

maxVersions :: Int
maxVersions = 30

data Session = Session
    { sConfig :: !SessionConfig
    , sState  :: !(MVar St)
    }

newSession :: SessionConfig -> IO Session
newSession cfg = do
    s <- newSessionState cfg
    startAutosaver s
    return s

newSessionState :: SessionConfig -> IO Session
newSessionState cfg = Session cfg <$> newMVar St
    { stProject  = newProject (sessionInitialScore cfg)
    , stNextJob  = 1
    , stRunning  = Nothing
    , stFinished = Nothing
    , stFailure  = Nothing
    , stAudio    = Nothing
    , stLoaded   = Nothing
    , stWatcher  = Nothing
    , stCue      = 0
    , stLoop     = Nothing
    , stVolume   = 1
    , stLost     = False
    , stValidSince = Revision 0
    , stVersions = []
    , stNextVersion = 1
    , stAudition = Nothing
    , stNextAudition = 1
    , stStems = emptyStems
    , stExport = Nothing
    , stNextExport = 1
    , stFile = Nothing
    , stSavedRevision = Revision 0
    , stHistory = emptyHistory
    , stAutosaved = Nothing
    , stRecoveryPending = False
    , stAutosaver = Nothing
    }

-- | Stop rendering and playback and release the audio device.
closeSession :: Session -> IO ()
closeSession s = do
  _ <- try (autosaveNow s) :: IO (Either SomeException ())
  modifyMVar_ (sState s) $ \st -> do
    forM_ (stRunning st) (killThread . jobThread)
    forM_ (stWatcher st) killThread
    forM_ (stAudition st >>= auWorker) killThread
    forM_ (stExport st) (killThread . exportThread)
    forM_ (stAutosaver st) killThread
    forM_ (stAudio st) backendClose
    return st { stRunning = Nothing, stWatcher = Nothing, stAudio = Nothing, stLoaded = Nothing, stAudition = Nothing }

-------------------------------------------------------------------------------
-- Project

projectView :: Session -> IO Value
projectView s = withMVar (sState s) projectViewLocked

projectViewLocked :: St -> IO Value
projectViewLocked st = do
    transport <- transportViewLocked st
    let p = stProject st
        declared = maybe mempty (scorePatches . validAst) (projectValidScore p)
        trackView t = object
            [ "id"               .= trackId t
            , "name"             .= trackScoreName t
            , "instrument"       .= trackScoreInstrument t
            , "patchSource"      .= sourceText (snd (resolvePatch declared t))
            , "patch"            .= fst (resolvePatch declared t)
            , "hasPatchOverride" .= isJust (trackPatchOverride t)
            , "mix"              .= trackMix t
            ]
    return $ object
        [ "revision"       .= projectRevision p
        , "scoreText"      .= projectScoreText p
        , "scoreIsCurrent" .= scoreIsCurrent p
        , "diagnostics"    .= projectDiagnostics p
        , "tracks"         .= map trackView (projectTracks p)
        , "song"           .= fmap (songInfo . validAst) (projectValidScore p)
        , "outline"        .= outline (projectScoreText p)
        , "timingVersion"  .= stValidSince st
        , "master"         .= projectMaster p
        , "versions"       .= map (versionView (projectScoreText p)) (stVersions st)
        , "history"        .= object ["undo" .= undoLabel (stHistory st), "redo" .= redoLabel (stHistory st)]
        , "file"           .= object
            [ "path" .= stFile st
            , "name" .= fmap takeFileName (stFile st)
            , "modified" .= (projectRevision p /= stSavedRevision st) ]
        , "generator"      .= object ["roots" .= rootNames, "scales" .= scaleNames]
        , "render"         .= renderView st
        , "transport"      .= transport
        ]
  where
    sourceText src = case src of
        FromOverride        -> "override" :: Text
        FromScoreInstrument -> "score"
        FromPreset          -> "preset"
        FromDefault         -> "default"

renderView :: St -> Value
renderView st =
    let inputs = renderInputs (stProject st)
        current f = finInputs f == inputs
        failedNow = maybe False ((== inputs) . failInputs) (stFailure st)
        state :: Text
        state
            | isJust (stRunning st)                 = "rendering"
            | failedNow                             = "failed"
            | maybe False current (stFinished st)   = "ready"
            | isJust (stFinished st)                = "outdated"
            | otherwise                             = "idle"
    in object
        [ "state"   .= state
        , "running" .= fmap (\j -> object ["jobId" .= jobId j, "revision" .= jobRevision j]) (stRunning st)
        , "latest"  .= fmap (\f -> finishedJson f (current f)) (stFinished st)
        , "failure" .= fmap (\f -> object ["jobId" .= failJobId f, "revision" .= failRevision f, "message" .= failMessage f]) (stFailure st)
        ]

finishedJson :: Finished -> Bool -> Value
finishedJson f isCurrent = object
    [ "jobId"           .= finJobId f
    , "revision"        .= finRevision f
    , "durationSeconds" .= durationSeconds f
    , "renderMillis"    .= finMillis f
    , "current"         .= isCurrent
    ]

durationSeconds :: Finished -> Double
durationSeconds f = fromIntegral (V.length (finFrames f)) / fromIntegral (finRate f)

-- | Replace the score text. Starts a render when the playable song changed.
setScore :: Session -> Emit -> Text -> IO Value
setScore s emit txt = either (error "SetScoreText cannot fail") id <$> applyProjectCommand s emit (SetScoreText txt)

applyProjectCommand :: Session -> Emit -> Command -> IO (Either ServiceError Value)
applyProjectCommand s emit cmd = modifyMVar (sState s) $ \st ->
    applyLocked s emit cmd st >>= \r -> case r of
        Left e -> return (st, Left e)
        Right st' -> do
            v <- projectViewLocked st'
            return (st', Right v)

-- | Apply a command and start a render when the playable song changed.
applyLocked :: Session -> Emit -> Command -> St -> IO (Either ServiceError St)
applyLocked s emit cmd st = applyAs s emit (describeChange (stProject st) cmd) cmd st

-- | Apply a command, noting it in the history as a given change.
applyAs :: Session -> Emit -> Change -> Command -> St -> IO (Either ServiceError St)
applyAs s emit change cmd st = case applyCommand cmd (stProject st) of
    Left (CommandError code msg params) ->
        let ecode = if code == "unknown_track" then UnknownTrack else InvalidValue
            detail = if null params then Nothing else Just (object ["params" .= params])
        in return (Left (ServiceError ecode msg detail))
    Right p -> do
        now <- getMonotonicTime
        Right <$> showProject s emit p st { stHistory = record now change (stProject st) p (stHistory st) }

-- | Make a project current and render it if its playable song changed.
showProject :: Session -> Emit -> Project -> St -> IO St
showProject s emit p st = do
    let changed = fmap validText (projectValidScore p) /= fmap validText (projectValidScore (stProject st))
        since = if changed then projectRevision p else stValidSince st
    startRenderLocked s emit False st { stProject = p, stValidSince = since }

-- | How a command reads in the history ("Undo typing"), and which commands
-- merge when they follow each other quickly.
describeChange :: Project -> Command -> Change
describeChange p cmd = case cmd of
    SetScoreText _ -> Change "typing" (Just "score") 2
    SetTrackPatch tid _ -> Change ("sound change on " <> name tid) (Just ("patch:" <> trackIdText tid)) 1.5
    ResetTrackPatch tid -> Change ("going back to the score's sound on " <> name tid) Nothing 0
    UpdateTrackMix tid (MixUpdate g pn m so)
        | isJust m -> Change ((if m == Just True then "muting " else "unmuting ") <> name tid) Nothing 0
        | isJust so -> Change ((if so == Just True then "soloing " else "unsoloing ") <> name tid) Nothing 0
        | isJust g -> Change ("level change on " <> name tid) (Just ("gain:" <> trackIdText tid)) 1.5
        | isJust pn -> Change ("pan change on " <> name tid) (Just ("pan:" <> trackIdText tid)) 1.5
        | otherwise -> Change "mix change" Nothing 0
    SetMasterGain _ -> Change "master level change" (Just "master") 1.5
    AdoptRetiredSettings _ to -> Change ("moving settings to " <> name to) Nothing 0
  where
    name tid = maybe "a track" trackScoreName (findTrack tid p)

-- | Step back (or forward) through the history.
undoChange, redoChange :: Session -> Emit -> IO Value
undoChange s emit = travel s emit undo
redoChange s emit = travel s emit redo

travel :: Session -> Emit -> (Project -> History -> Maybe (Project, History)) -> IO Value
travel s emit step = modifyMVar (sState s) $ \st -> case step (stProject st) (stHistory st) of
    Nothing -> (,) st <$> projectViewLocked st
    Just (p, h) -> do
        let p' = p { projectRevision = nextRevision (projectRevision (stProject st)) }
        st' <- showProject s emit p' st { stHistory = h }
        v <- projectViewLocked st'
        return (st', v)

versionView :: Text -> ScoreVersion -> Value
versionView current v = object
    [ "id"       .= versionId v
    , "label"    .= versionLabel v
    , "revision" .= versionRevision v
    , "settings" .= versionSettings v
    , "current"  .= (versionText v == current)
    , "lines"    .= length (T.lines (versionText v))
    ]

-- | Keep the text being replaced, unless it is blank or already the newest version.
keepCurrent :: Text -> St -> St
keepCurrent label st =
    let p = stProject st
        txt = projectScoreText p
        known = case stVersions st of
            (v : _) -> versionText v == txt
            [] -> False
    in if T.null (T.strip txt) || known then st else pushVersion (ScoreVersion 0 label txt (projectRevision p) Nothing) st

pushVersion :: ScoreVersion -> St -> St
pushVersion v st = st
    { stVersions = take maxVersions (v { versionId = stNextVersion st } : stVersions st)
    , stNextVersion = stNextVersion st + 1
    }

-- | Replace the score with a generated one. The replaced text and the
-- generated one are both kept as versions.
generate :: Session -> Emit -> GenSettings -> IO (Either ServiceError Value)
generate s emit g = case validateSettings g of
    problems@(_ : _) -> return . Left $ ServiceError InvalidValue "The generator settings are not valid"
        (Just (object ["params" .= [ ParamError k m | (k, m) <- problems ]]))
    [] -> do
        let txt = printSong (generateScore g)
        replaceScore s emit "generating" txt $ \st ->
            let kept = keepCurrent "Before generating" st
                again = map versionText (take 1 (stVersions kept)) == [txt]
            in \rev -> if again then kept
                       else pushVersion (ScoreVersion 0 ("Generated: " <> settingsLabel g) txt rev (Just g)) kept

-- | Bring back a kept version; the text it replaces is kept first.
restoreVersion :: Session -> Emit -> Int -> IO (Either ServiceError Value)
restoreVersion s emit vid = do
    found <- withMVar (sState s) (\st -> return (filter ((== vid) . versionId) (stVersions st)))
    case found of
        [] -> return (Left (ServiceError InvalidValue "There is no such version" Nothing))
        (v : _) -> replaceScore s emit "restoring a version" (versionText v) $ \st ->
            let kept = keepCurrent ("Before restoring " <> T.toLower (T.take 1 (versionLabel v)) <> T.drop 1 (versionLabel v)) st
            in const kept

-- | Set the score text and update the kept versions in one step. @keep@
-- sees the state before the change and the revision after it; only its
-- versions are used.
replaceScore :: Session -> Emit -> Text -> Text -> (St -> Revision -> St) -> IO (Either ServiceError Value)
replaceScore s emit what txt keep = modifyMVar (sState s) $ \st ->
    applyAs s emit (Change what Nothing 0) (SetScoreText txt) st >>= \r -> case r of
        Left e -> return (st, Left e)
        Right st' -> do
            let versions = keep st (projectRevision (stProject st'))
                kept = st' { stVersions = stVersions versions, stNextVersion = stNextVersion versions }
            v <- projectViewLocked kept
            return (kept, Right v)

-------------------------------------------------------------------------------
-- Export

exportRates :: [Int]
exportRates = [44100, 48000, 96000]

-- | Render the project as it is now to a WAV file. The revision is captured
-- when the export starts; later edits do not change the file. The file
-- appears only when complete: it is written next to its final name first.
startExport :: Session -> Emit -> ExportRequest -> IO (Either ServiceError Value)
startExport s emit req = modifyMVar (sState s) $ \st -> do
    dirOk <- doesDirectoryExist (takeDirectory (exportPath req))
    folderInTheWay <- doesDirectoryExist (exportPath req)
    let problems = concat
            [ [ ParamError "path" "Choose a file name ending in .wav" | map toLower' (takeExtension (exportPath req)) /= ".wav" ]
            , [ ParamError "path" ("The folder " <> T.pack (takeDirectory (exportPath req)) <> " does not exist") | not dirOk ]
            , [ ParamError "path" "There is a folder with this name; choose another file name" | folderInTheWay ]
            , [ ParamError "sampleRate" "Use 44100, 48000 or 96000 Hz" | exportRate req `notElem` exportRates ]
            , [ ParamError "bitDepth" "Use 16, 24 or 32 bits" | exportBitDepth req `notElem` [16, 24, 32] ]
            ]
    case (problems, compileProject (stProject st), stExport st) of
        (_ : _, _, _) -> return (st, Left (invalidParams problems))
        (_, _, Just _) -> return (st, Left (ServiceError InvalidValue "An export is already running" Nothing))
        (_, Nothing, _) -> return (st, Left (serviceError NoRender "There is nothing to export: the score has never been valid"))
        ([], Just compiled, Nothing) -> do
            let eid = stNextExport st
                rev = compiledRevision compiled
                part = exportPath req ++ ".part"
                rate = exportRate req
                -- Stems at the playback rate are reused; other rates render afresh.
                stems = if rate == sessionSampleRate (sConfig s) then stStems st else emptyStems
            emit (Event "export.started" (object ["exportId" .= eid, "revision" .= rev, "path" .= exportPath req]))
            tid <- forkIO (runExport s emit eid rev req part stems compiled)
            return ( st { stExport = Just (ExportJob eid tid part), stNextExport = eid + 1 }
                   , Right (object ["exportId" .= eid, "revision" .= rev]) )
  where
    toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

runExport :: Session -> Emit -> Int -> Revision -> ExportRequest -> FilePath -> Stems -> CompiledProject -> IO ()
runExport s emit eid rev req part stems compiled = do
    let rate = exportRate req
        depth = case exportBitDepth req of
            16 -> BitDepth16
            24 -> BitDepth24
            _  -> BitDepth32Float
        progress done total = emit $ Event "export.progress" $ object
            ["exportId" .= eid, "completed" .= done, "total" .= (total + 1)]
    result <- try $ do
        (_, frames) <- sessionRenderer (sConfig s) rate progress stems compiled
        let bytes = encodeWAV depth defaultConfig { configSampleRate = rate } frames
        BL.writeFile part bytes
        renameFile part (exportPath req)
        return (V.length frames, BL.length bytes)
    case result of
        Left (e :: SomeException) | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        _ -> modifyMVar_ (sState s) $ \st -> do
            case result of
                Right (frames, size) -> emit $ Event "export.finished" $ object
                    [ "exportId" .= eid, "revision" .= rev, "path" .= exportPath req
                    , "sampleRate" .= rate, "bitDepth" .= exportBitDepth req, "frames" .= frames
                    , "seconds" .= (fromIntegral frames / fromIntegral rate :: Double), "bytes" .= size ]
                Left e -> do
                    _ <- try (removeFile part) :: IO (Either SomeException ())
                    emit $ Event "export.failed" $ object
                        ["exportId" .= eid, "message" .= exportFailure (exportPath req) e]
            return st { stExport = Nothing }

isInappropriate :: IOError -> Bool
isInappropriate io = case ioeGetErrorType io of
    InappropriateType -> True
    _ -> False

-- | Words for an export failure that say what to do about it.
exportFailure :: FilePath -> SomeException -> Text
exportFailure = writeFailure "The export failed"

-- | Words for a failure to write a file that say what to do about it.
writeFailure :: Text -> FilePath -> SomeException -> Text
writeFailure failed path e = case fromException e of
    Just io
        | isPermissionError io -> "Sonora is not allowed to write " <> T.pack path <> ". Choose another folder, or check the file is not read-only."
        | isAlreadyInUseError io -> T.pack path <> " is open in another program. Close it there, or choose another name."
        | isFullError io -> "The disk is full. Free some space, or choose another drive."
        | isDoesNotExistError io -> "The folder for " <> T.pack path <> " no longer exists. Choose another folder."
        | isInappropriate io -> "Sonora could not write " <> T.pack path <> " (" <> plainMessage e <> "). Choose another file name."
    _ -> failed <> ": " <> plainMessage e <> ". Try again, or choose another folder."

cancelExport :: Session -> Emit -> IO Value
cancelExport s emit = modifyMVar (sState s) $ \st -> case stExport st of
    Nothing -> return (st, object ["cancelled" .= False])
    Just job -> do
        killThread (exportThread job)
        _ <- try (removeFile (exportPart job)) :: IO (Either SomeException ())
        emit (Event "export.cancelled" (object ["exportId" .= exportId job]))
        return (st { stExport = Nothing }, object ["cancelled" .= True])

-------------------------------------------------------------------------------
-- Project files

-- | Checks for a file about to be written.
targetProblems :: String -> Text -> FilePath -> IO [ParamError]
targetProblems ext kind path = do
    dirOk <- doesDirectoryExist (takeDirectory path)
    folder <- doesDirectoryExist path
    return $ concat
        [ [ ParamError "path" ("Choose a file name ending in " <> T.pack ext <> " for " <> kind) | map toLowerAscii (takeExtension path) /= ext ]
        , [ ParamError "path" ("The folder " <> T.pack (takeDirectory path) <> " does not exist") | not dirOk ]
        , [ ParamError "path" "There is a folder with this name; choose another file name" | folder ]
        ]

toLowerAscii :: Char -> Char
toLowerAscii c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Save the whole project. Without a path, to where it was opened or last
-- saved. A failed save leaves any existing file as it was.
saveProject :: Session -> Maybe FilePath -> IO (Either ServiceError Value)
saveProject s requested = modifyMVar (sState s) $ \st -> case requested `orElse` stFile st of
    Nothing -> return (st, Left (invalidParams [ParamError "path" "Choose where to save the project"]))
    Just path -> do
        problems <- targetProblems projectExtension "a project" path
        if not (null problems) then return (st, Left (invalidParams problems)) else do
            let p = stProject st
                file = ProjectFile p [ SavedVersion (versionLabel v) (versionText v) (versionSettings v) | v <- stVersions st ]
            result <- try (writeAtomically path (encodeProject file))
            case result of
                Left e -> return (st, Left (serviceError FileError (writeFailure "Saving failed" path e)))
                Right () -> do
                    let st' = st { stFile = Just path, stSavedRevision = projectRevision p }
                    v <- projectViewLocked st'
                    return (st', Right v)
  where
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Replace the session's project with an opened file's.
openProject :: Session -> Emit -> FilePath -> IO (Either ServiceError Value)
openProject s emit path = do
    loaded <- readProjectFile path
    case loaded of
        Left e -> return . Left $ serviceError (case e of Unreadable _ -> FileError; _ -> InvalidProject) (loadErrorMessage e)
        Right (ProjectFile p versions) -> fmap Right . modifyMVar (sState s) $ \st -> do
            st' <- replaceProject s emit (Just path) p versions st
            v <- projectViewLocked st'
            return (st', v)

-- | Start again from the starter score, untitled.
startNewProject :: Session -> Emit -> IO Value
startNewProject s emit = modifyMVar (sState s) $ \st -> do
    st' <- replaceProject s emit Nothing (newProject (sessionInitialScore (sConfig s))) [] st
    v <- projectViewLocked st'
    return (st', v)

-- | Stop what belongs to the old project and take on a new one. Revisions
-- keep counting up, so views of the new project are always newer.
replaceProject :: Session -> Emit -> Maybe FilePath -> Project -> [SavedVersion] -> St -> IO St
replaceProject s emit path p versions st0 = do
    st1 <- cancelRunningLocked emit st0
    forM_ (stAudition st1 >>= auWorker) killThread
    forM_ (stAudio st1) $ \b -> do
        backendSetPlaying b SongVoice False
        backendSetPlaying b AuditionVoice False
        backendSetLoop b SongVoice Nothing
    let rev = nextRevision (projectRevision (stProject st1))
        kept = zipWith (\i (SavedVersion l t g) -> ScoreVersion i l t rev g) [1 ..] (take maxVersions versions)
        st2 = st1
            { stProject = p { projectRevision = rev }
            , stFinished = Nothing, stFailure = Nothing, stLoaded = Nothing
            , stCue = 0, stLoop = Nothing
            , stStems = emptyStems
            , stAudition = Nothing
            , stVersions = kept, stNextVersion = length kept + 1
            , stFile = path, stSavedRevision = rev
            , stValidSince = rev
            , stHistory = emptyHistory
            }
    st3 <- startRenderLocked s emit False st2
    v <- transportViewLocked st3
    emit (Event "transport" v)
    return st3

-- | Read a .sonora score into the project, keeping the score it replaces.
importScore :: Session -> Emit -> FilePath -> IO (Either ServiceError Value)
importScore s emit path = do
    result <- try $ do
        size <- getFileSize path
        if size > 4 * 1024 * 1024 then return Nothing else Just <$> BL.readFile path
    case result of
        Left (e :: SomeException) -> return (Left (serviceError FileError ("The score could not be read: " <> plainMessage e)))
        Right Nothing -> return (Left (serviceError FileError "The file is too large to be a score."))
        Right (Just bytes) -> case TE.decodeUtf8' (BL.toStrict bytes) of
            Left _ -> return (Left (serviceError FileError "The file is not a text score (it is not UTF-8 text)."))
            Right txt -> do
                let clean = maybe txt id (T.stripPrefix "\xFEFF" txt)
                replaceScore s emit "importing a score" clean $ \st -> const (keepCurrent ("Before importing " <> T.pack (takeFileName path)) st)

-- | Write the score text, exactly as edited, to a .sonora file.
exportScore :: Session -> FilePath -> IO (Either ServiceError Value)
exportScore s path = do
    problems <- targetProblems scoreExtension "a score" path
    if not (null problems) then return (Left (invalidParams problems)) else do
        txt <- withMVar (sState s) (return . projectScoreText . stProject)
        result <- try (writeAtomically path (BL.fromStrict (TE.encodeUtf8 txt)))
        return $ case result of
            Left e -> Left (serviceError FileError (writeFailure "Saving the score failed" path e))
            Right () -> Right (object ["path" .= path])

-------------------------------------------------------------------------------
-- Recovery

-- | Begin keeping unsaved work safe. An autosave already in the recovery
-- folder is left alone until it is restored or discarded.
startAutosaver :: Session -> IO ()
startAutosaver s = case sessionRecoveryDir (sConfig s) of
    Nothing -> return ()
    Just dir -> do
        found <- findAutosave dir
        tid <- forkIO (forever (threadDelay (sessionAutosaveMicros (sConfig s)) >> autosaveNow s))
        modifyMVar_ (sState s) $ \st -> return st
            { stRecoveryPending = found /= NoAutosave, stAutosaver = Just tid }

-- | Write unsaved work if it changed since the last autosave, or remove the
-- autosave once nothing is unsaved. The file is written outside the lock.
autosaveNow :: Session -> IO ()
autosaveNow s = case sessionRecoveryDir (sConfig s) of
    Nothing -> return ()
    Just dir -> do
        todo <- modifyMVar (sState s) $ \st -> do
            let p = stProject st
                rev = projectRevision p
                modified = rev /= stSavedRevision st
                file = ProjectFile p [ SavedVersion (versionLabel v) (versionText v) (versionSettings v) | v <- stVersions st ]
            return $ if stRecoveryPending st then (st, return ())
                else if modified && stAutosaved st /= Just rev
                    then (st { stAutosaved = Just rev }, writeOrForget (writeAutosave dir (stFile st) file))
                else if not modified && isJust (stAutosaved st)
                    then (st { stAutosaved = Nothing }, removeAutosave dir)
                else (st, return ())
        todo
  where
    -- A failed write is tried again on the next round.
    writeOrForget act = do
        r <- try act
        case r of
            Left (_ :: SomeException) -> modifyMVar_ (sState s) (\st -> return st { stAutosaved = Nothing })
            Right () -> return ()

-- | Whether work from an earlier session can be recovered. An autosave that
-- cannot be read is moved aside and reported, never deleted.
checkRecovery :: Session -> IO Value
checkRecovery s = case sessionRecoveryDir (sConfig s) of
    Nothing -> return (object ["available" .= False])
    Just dir -> modifyMVar (sState s) $ \st ->
        if not (stRecoveryPending st) then return (st, object ["available" .= False]) else do
            found <- findAutosave dir
            case found of
                NoAutosave -> return (st { stRecoveryPending = False }, object ["available" .= False])
                Recoverable info (ProjectFile p _) -> return (st, object
                    [ "available" .= True
                    , "file" .= recoveryFile info
                    , "name" .= fmap takeFileName (recoveryFile info)
                    , "savedAt" .= recoverySaved info
                    , "title" .= fmap (astSongTitle . validAst) (projectValidScore p) ])
                Unrecoverable e -> do
                    aside <- setAsideAutosave dir
                    return (st { stRecoveryPending = False }, object
                        [ "available" .= False
                        , "problem" .= ("Unsaved work from last time could not be read (" <> loadErrorMessage e <> ")"
                            <> maybe "" (\a -> " It was kept as " <> T.pack a <> ".") aside) ])

-- | Bring back the work from the autosave. It stays unsaved until saved.
restoreRecovery :: Session -> Emit -> IO (Either ServiceError Value)
restoreRecovery s emit = case sessionRecoveryDir (sConfig s) of
    Nothing -> return (Left (serviceError InvalidValue "There is nothing to recover"))
    Just dir -> do
        found <- findAutosave dir
        case found of
            Recoverable info (ProjectFile p versions) -> fmap Right . modifyMVar (sState s) $ \st -> do
                st1 <- replaceProject s emit (recoveryFile info) p versions st
                let st2 = st1 { stRecoveryPending = False, stSavedRevision = Revision (-1), stAutosaved = Nothing }
                v <- projectViewLocked st2
                return (st2, v)
            Unrecoverable e -> return (Left (serviceError InvalidProject (loadErrorMessage e)))
            NoAutosave -> return (Left (serviceError InvalidValue "There is nothing to recover"))

-- | Drop unsaved work: the autosave from before, or the current changes when
-- the user closes without saving.
discardRecovery :: Session -> IO Value
discardRecovery s = modifyMVar (sState s) $ \st -> do
    forM_ (sessionRecoveryDir (sConfig s)) removeAutosave
    return ( st { stRecoveryPending = False, stSavedRevision = projectRevision (stProject st), stAutosaved = Nothing }
           , object ["discarded" .= True] )

-------------------------------------------------------------------------------
-- Audition

auditionProblems :: Patch -> Int -> Double -> [ParamError]
auditionProblems patch pitch velocity = concat
    [ [ ParamError ("patch." <> paramPath e) (paramMessage e) | e <- validatePatch patch ]
    , [ ParamError "pitch" ("must be between " <> T.pack (show minPitch) <> " and " <> T.pack (show maxPitch))
      | pitch < minPitch || pitch > maxPitch ]
    , [ ParamError "velocity" "must be between 0 and 1" | not (finite velocity) || velocity < 0 || velocity > 1 ]
    ]

invalidParams :: [ParamError] -> ServiceError
invalidParams ps = ServiceError InvalidValue "Some values are not valid" (Just (object ["params" .= ps]))

-- | Start a note with a sound, held until released (at most 'holdSeconds').
-- Replaces any note already sounding.
startAudition :: Session -> Emit -> Patch -> Int -> Double -> IO (Either ServiceError Value)
startAudition s emit patch pitch velocity = case auditionProblems patch pitch velocity of
    problems@(_ : _) -> return (Left (invalidParams problems))
    [] -> modifyMVar (sState s) $ \st0 -> do
        st <- noticeLoss st0
        opened <- ensureAudio s st
        case opened of
            Left err -> return (st, Left (serviceError AudioUnavailable (T.pack (audioErrorMessage err))))
            Right (_, st1) -> do
                forM_ (stAudition st1 >>= auWorker) killThread
                let aid = stNextAudition st1
                    au = Audition aid patch (AuditionNote pitch velocity holdSeconds) 0 0 False Nothing
                au' <- renderAuditionLocked s emit au
                return ( st1 { stAudition = Just au', stNextAudition = aid + 1 }
                       , Right (object ["auditionId" .= aid]) )

-- | Hear a changed sound on the note that is sounding.
updateAudition :: Session -> Emit -> Int -> Patch -> IO (Either ServiceError Value)
updateAudition s emit aid patch = case [ ParamError ("patch." <> paramPath e) (paramMessage e) | e <- validatePatch patch ] of
    problems@(_ : _) -> return (Left (invalidParams problems))
    [] -> withAudition s aid $ \_ au -> do
        forM_ (auWorker au) killThread
        renderAuditionLocked s emit au { auPatch = patch }

-- | Let go of the key: the note releases from where it is now.
releaseAudition :: Session -> Emit -> Int -> IO (Either ServiceError Value)
releaseAudition s emit aid = withAudition s aid $ \st au ->
    if auReleased au then return au else do
        playhead <- case stAudio st of
            Just b | auShown au > 0 -> do
                status <- backendStatus b AuditionVoice
                return (Just (fromIntegral (statusPosition status) / fromIntegral (backendSampleRate b)))
            _ -> return Nothing
        -- Release a little ahead of the play head so the new render arrives
        -- before its release begins; a note not yet heard becomes a short tap.
        let gate = maybe tapSeconds (+ releaseLead) playhead
        if gate >= noteGate (auNote au)
            then return au { auReleased = True }
            else do
                forM_ (auWorker au) killThread
                renderAuditionLocked s emit au { auReleased = True, auNote = (auNote au) { noteGate = gate } }

-- | Silence the audition and forget it.
stopAudition :: Session -> Emit -> IO Value
stopAudition s _ = modifyMVar (sState s) $ \st -> do
    forM_ (stAudition st >>= auWorker) killThread
    forM_ (stAudio st) (\b -> backendSetPlaying b AuditionVoice False)
    return (st { stAudition = Nothing }, object ["stopped" .= True])

tapSeconds, releaseLead :: Double
tapSeconds = 0.12
releaseLead = 0.08

-- | Apply a change to the current audition; answers whether it is still the
-- current one.
-- | Apply a change to the current audition under the lock, so a render it
-- starts cannot finish before the change is recorded. Answers whether the
-- audition is still the current one.
withAudition :: Session -> Int -> (St -> Audition -> IO Audition) -> IO (Either ServiceError Value)
withAudition s aid f = modifyMVar (sState s) $ \st -> case stAudition st of
    Just au | auId au == aid -> do
        au' <- f st au
        return (st { stAudition = Just au' }, reply True)
    _ -> return (st, reply False)
  where
    reply active = Right (object ["auditionId" .= aid, "active" .= active])

-- | Render the next revision of a note in the background. The result is
-- played only if it is still the latest revision of the current audition.
renderAuditionLocked :: Session -> Emit -> Audition -> IO Audition
renderAuditionLocked s emit au = do
    let rev = auRevision au + 1
        sr = sessionSampleRate (sConfig s)
        aid = auId au
    tid <- forkIO $ do
        result <- try (sessionAuditionRenderer (sConfig s) sr (auPatch au) (auNote au))
        case result of
            Left (e :: SomeException) | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
            _ -> modifyMVar_ (sState s) $ \st -> case stAudition st of
                Just now | auId now == aid && auRevision now == rev -> case result of
                    Right frames -> do
                        case stAudio st of
                            Just b
                                | auShown now == 0 -> do
                                    backendLoad b AuditionVoice frames
                                    backendSetPlaying b AuditionVoice True
                                | otherwise -> backendReplace b AuditionVoice frames
                            Nothing -> return ()
                        emit $ Event "audition" $ object
                            [ "auditionId" .= aid, "revision" .= rev, "state" .= ("playing" :: Text)
                            , "seconds" .= (fromIntegral (V.length frames) / fromIntegral sr :: Double) ]
                        return st { stAudition = Just now { auShown = rev, auWorker = Nothing } }
                    Left e -> do
                        emit $ Event "audition" $ object
                            [ "auditionId" .= aid, "revision" .= rev, "state" .= ("failed" :: Text)
                            , "message" .= plainMessage e ]
                        return st { stAudition = Just now { auWorker = Nothing } }
                _ -> return st
    return au { auRevision = rev, auWorker = Just tid }

-------------------------------------------------------------------------------
-- Rendering

-- | Make sure a render of the current project exists or is under way;
-- retries a render that failed for the current inputs.
startRender :: Session -> Emit -> IO Value
startRender s emit = modifyMVar (sState s) $ \st -> do
    st' <- startRenderLocked s emit True st
    return (st', renderView st')

cancelRender :: Session -> Emit -> IO Value
cancelRender s emit = modifyMVar (sState s) $ \st -> do
    st' <- cancelRunningLocked emit st
    return (st', renderView st')

startRenderLocked :: Session -> Emit -> Bool -> St -> IO St
startRenderLocked s emit retryFailed st = case compileProject (stProject st) of
    Nothing -> return st
    Just compiled -> do
        let inputs = renderInputs (stProject st)
            matches f = f == inputs
            sameRunning = maybe False (matches . jobInputs) (stRunning st)
            upToDate = maybe False (matches . finInputs) (stFinished st)
            failedSame = not retryFailed && maybe False (matches . failInputs) (stFailure st)
        if | sameRunning -> return st
           -- Already rendered (or already failed) for these inputs: any job
           -- for other inputs is now pointless.
           | upToDate || failedSame -> cancelRunningLocked emit st
           | otherwise -> do
                st1 <- cancelRunningLocked emit st
                let jid = stNextJob st1
                    rev = projectRevision (stProject st1)
                emit (Event "render.started" (object ["jobId" .= jid, "revision" .= rev]))
                -- The worker reports back through the session lock, which we
                -- hold, so it cannot finish before it is registered.
                tid <- forkIO (runJob s emit jid rev inputs (stStems st1) compiled)
                return st1 { stNextJob = jid + 1, stRunning = Just (Job jid rev inputs tid), stFailure = Nothing }

cancelRunningLocked :: Emit -> St -> IO St
cancelRunningLocked emit st = case stRunning st of
    Nothing -> return st
    Just j -> do
        -- Killing asynchronously keeps the lock holder from blocking; a
        -- worker that still finishes finds itself superseded and is ignored.
        void (forkIO (killThread (jobThread j)))
        emit (Event "render.cancelled" (object ["jobId" .= jobId j, "revision" .= jobRevision j]))
        return st { stRunning = Nothing }

runJob :: Session -> Emit -> Int -> Revision -> RenderInputs -> Stems -> CompiledProject -> IO ()
runJob s emit jid rev inputs stems compiled = do
    t0 <- getMonotonicTime
    let progress done total = emit $ Event "render.progress" $ object
            ["jobId" .= jid, "revision" .= rev, "completed" .= done, "total" .= total]
    result <- try (sessionRenderer (sConfig s) (sessionSampleRate (sConfig s)) progress stems compiled)
    t1 <- getMonotonicTime
    case result of
        Left (e :: SomeException) | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        _ -> modifyMVar_ (sState s) $ \st ->
            if fmap jobId (stRunning st) /= Just jid
                then return st  -- superseded while finishing
                else case result of
                    Right (stems', frames) -> do
                        let fin = Finished jid rev inputs frames (sessionSampleRate (sConfig s)) (round ((t1 - t0) * 1000))
                            isCurrent = inputs == renderInputs (stProject st)
                        emit (Event "render.finished" (finishedJson fin isCurrent))
                        swapIn emit st { stRunning = Nothing, stFinished = Just fin, stStems = stems' }
                    Left e -> do
                        let msg = plainMessage e
                        emit $ Event "render.failed" $ object ["jobId" .= jid, "revision" .= rev, "message" .= msg]
                        return st { stRunning = Nothing, stFailure = Just (Failure jid rev inputs msg) }

-- | Replace the loaded render with the latest one at the same position.
swapIn :: Emit -> St -> IO St
swapIn emit st = case (stAudio st, stLoaded st, stFinished st) of
    (Just b, Just loaded, Just fin) | finJobId loaded /= finJobId fin -> do
        backendReplace b SongVoice (finFrames fin)
        let st' = st { stLoaded = Just fin }
        applyLoop st'
        emitTransport emit st'
        return st'
    _ -> return st

-------------------------------------------------------------------------------
-- Timing

-- | When each line of the last valid score sounds, tagged with the
-- @timingVersion@ of the project view it belongs to.
scoreTiming :: Session -> IO Value
scoreTiming s = do
    st <- readMVar (sState s)
    return $ object
        [ "version" .= stValidSince st
        , "tracks"  .= maybe [] id (timing (stProject st))
        ]

-------------------------------------------------------------------------------
-- Waveform

maxPeakBuckets :: Int
maxPeakBuckets = 4096

-- | A bounded overview of the latest finished render, as @[min, max]@ pairs.
renderPeaks :: Session -> Int -> IO (Either ServiceError Value)
renderPeaks s buckets
    | buckets < 1 || buckets > maxPeakBuckets = return $ Left $ serviceError InvalidValue
        ("\"buckets\" must be between 1 and " <> T.pack (show maxPeakBuckets))
    | otherwise = do
        finished <- stFinished <$> readMVar (sState s)
        return $ case finished of
            Nothing -> Left (serviceError NoRender "Nothing has been rendered yet")
            Just fin -> Right $ object
                [ "jobId"           .= finJobId fin
                , "durationSeconds" .= durationSeconds fin
                , "peaks"           .= [ [milli lo, milli hi] | (lo, hi) <- waveformPeaks buckets (finFrames fin) ]
                ]
  where
    milli :: Double -> Double
    milli x = fromIntegral (round (x * 1000) :: Int) / 1000

-------------------------------------------------------------------------------
-- Transport

-- | The shortest accepted loop.
minLoopSeconds :: Double
minLoopSeconds = 0.05

transportView :: Session -> IO Value
transportView s = withMVar (sState s) transportViewLocked

transportViewLocked :: St -> IO Value
transportViewLocked st = case (stAudio st, stLoaded st) of
    (Just b, Just fin) -> do
        status <- backendStatus b SongVoice
        heard <- backendHeard b SongVoice
        lost <- backendDeviceLost b
        let atEnd = statusLength status > 0 && statusPosition status >= statusLength status
            -- The song has ended for the mixer but its last moments are
            -- still on their way to the speaker.
            finishing = not (statusPlaying status) && heardPlaying heard && atEnd
        return $ view (not lost && (statusPlaying status || finishing))
            (toSeconds b (heardPosition heard)) (Just fin) (stLost st || lost)
    _ -> return $ view False (stCue st) (stFinished st) (stLost st)
  where
    view :: Bool -> Double -> Maybe Finished -> Bool -> Value
    view playing position render lost = object
        [ "playing"         .= playing
        , "positionSeconds" .= position
        , "durationSeconds" .= maybe 0 durationSeconds render
        , "jobId"           .= fmap finJobId (stLoaded st)
        , "revision"        .= fmap finRevision (stLoaded st)
        , "current"         .= maybe False ((== renderInputs (stProject st)) . finInputs) (stLoaded st)
        , "loop"            .= fmap (\(a, b) -> object ["start" .= a, "end" .= b]) (stLoop st)
        , "loopActive"      .= maybe False (\f -> isJust (loopFrames f (stLoop st))) render
        , "volume"          .= stVolume st
        , "deviceLost"      .= lost
        ]

emitTransport :: Emit -> St -> IO ()
emitTransport emit st = transportViewLocked st >>= emit . Event "transport"

toSeconds :: AudioBackend -> Word64 -> Double
toSeconds b frames = fromIntegral frames / fromIntegral (backendSampleRate b)

-- | A loop range in frames of a render; Nothing if too little of it is left.
loopFrames :: Finished -> Maybe (Double, Double) -> Maybe (Word64, Word64)
loopFrames fin range = do
    (a, b) <- range
    let rate = fromIntegral (finRate fin) :: Double
        len = fromIntegral (V.length (finFrames fin)) :: Word64
        from = round (a * rate)
        to = min len (round (b * rate))
    if to > from && fromIntegral (to - from) >= minLoopSeconds * rate * 0.99 then Just (from, to) else Nothing

applyLoop :: St -> IO ()
applyLoop st = case (stAudio st, stLoaded st) of
    (Just b, Just fin) -> backendSetLoop b SongVoice (loopFrames fin (stLoop st))
    _ -> return ()

-- | Play the latest finished render from the current position, or from the
-- start if the position is at the end.
play :: Session -> Emit -> IO (Either ServiceError Value)
play s emit = modifyMVar (sState s) $ \st0 -> do
    st <- noticeLoss st0
    case stFinished st of
        Nothing -> return (st, Left (noRender st))
        Just fin -> do
            opened <- ensureAudio s st
            case opened of
                Left err -> return (st, Left (serviceError AudioUnavailable (T.pack (audioErrorMessage err))))
                Right (b, st1) -> do
                    st2 <- ensureLoaded b fin st1
                    status <- backendStatus b SongVoice
                    when (statusPosition status >= statusLength status) $ backendSeek b SongVoice 0
                    backendSetPlaying b SongVoice True
                    st3 <- ensureWatcher s emit st2
                    v <- transportViewLocked st3
                    emit (Event "transport" v)
                    return (st3, Right v)

noRender :: St -> ServiceError
noRender st = serviceError NoRender
    (if isJust (stRunning st) then "The song is still rendering" else "Nothing has been rendered yet")

pause :: Session -> Emit -> IO Value
pause s emit = transportOp s emit $ \st -> st <$ onLoaded st (\b -> backendSetPlaying b SongVoice False)

stop :: Session -> Emit -> IO Value
stop s emit = transportOp s emit $ \st -> do
    onLoaded st $ \b -> backendSetPlaying b SongVoice False >> backendSeek b SongVoice 0
    return st { stCue = 0 }

-- | Move to a position in seconds; past the end means the end.
seek :: Session -> Emit -> Double -> IO (Either ServiceError Value)
seek s emit seconds
    | not (finite seconds) || seconds < 0 = invalid "\"seconds\" must be a non-negative number"
    | otherwise = modifyMVar (sState s) $ \st0 -> do
        st <- noticeLoss st0
        case stFinished st of
            Nothing -> return (st, Left (noRender st))
            Just fin -> do
                let target = min seconds (durationSeconds fin)
                    st' = st { stCue = target }
                onLoaded st' $ \b -> backendSeek b SongVoice (round (target * fromIntegral (backendSampleRate b)))
                v <- transportViewLocked st'
                emit (Event "transport" v)
                return (st', Right v)

-- | Set or clear the loop range in seconds. The part beyond the end of the
-- song is ignored; a loop with too little left is kept but inactive.
setLoop :: Session -> Emit -> Maybe (Double, Double) -> IO (Either ServiceError Value)
setLoop s emit range = case range of
    Just (a, b)
        | not (finite a && finite b) || a < 0 -> invalid "Loop positions must be non-negative numbers"
        | b - a < minLoopSeconds -> invalid ("A loop must be at least " <> T.pack (show minLoopSeconds) <> " seconds long")
    _ -> fmap Right $ transportOp s emit $ \st -> do
        let st' = st { stLoop = range }
        applyLoop st'
        return st'

-- | Set the listening volume, 0 to 1.
setVolume :: Session -> Emit -> Double -> IO (Either ServiceError Value)
setVolume s emit volume
    | not (finite volume) || volume < 0 || volume > 1 = invalid "\"volume\" must be between 0 and 1"
    | otherwise = fmap Right $ transportOp s emit $ \st -> do
        forM_ (stAudio st) (`backendSetMasterGain` volume)
        return st { stVolume = volume }

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

invalid :: Text -> IO (Either ServiceError a)
invalid = return . Left . serviceError InvalidValue

transportOp :: Session -> Emit -> (St -> IO St) -> IO Value
transportOp s emit op = modifyMVar (sState s) $ \st -> do
    st' <- noticeLoss st >>= op
    v <- transportViewLocked st'
    emit (Event "transport" v)
    return (st', v)

onLoaded :: St -> (AudioBackend -> IO ()) -> IO ()
onLoaded st act = case (stAudio st, stLoaded st) of
    (Just b, Just _) -> act b
    _ -> return ()

-- | If the device went away, release it and remember where playback was.
noticeLoss :: St -> IO St
noticeLoss st = case stAudio st of
    Nothing -> return st
    Just b -> do
        lost <- backendDeviceLost b
        if not lost then return st else do
            cue <- if isJust (stLoaded st)
                then toSeconds b . heardPosition <$> backendHeard b SongVoice
                else return (stCue st)
            backendClose b
            return st { stAudio = Nothing, stLoaded = Nothing, stCue = cue, stLost = True }

-- | An open audio backend. Call 'noticeLoss' first.
ensureAudio :: Session -> St -> IO (Either AudioError (AudioBackend, St))
ensureAudio s st = case stAudio st of
    Just b -> return (Right (b, st))
    Nothing -> do
        opened <- sessionOpenAudio (sConfig s) (sessionSampleRate (sConfig s))
        case opened of
            Left err -> return (Left err)
            Right b -> do
                backendSetMasterGain b (stVolume st)
                return (Right (b, st { stAudio = Just b, stLoaded = Nothing, stLost = False }))

-- | Load a render onto the song voice, continuing from the current position.
ensureLoaded :: AudioBackend -> Finished -> St -> IO St
ensureLoaded b fin st
    | fmap finJobId (stLoaded st) == Just (finJobId fin) = return st
    | otherwise = do
        if isJust (stLoaded st)
            then backendReplace b SongVoice (finFrames fin)
            else do
                backendLoad b SongVoice (finFrames fin)
                backendSeek b SongVoice (round (stCue st * fromIntegral (backendSampleRate b)))
        let st' = st { stLoaded = Just fin }
        applyLoop st'
        return st'

-- | Emit @transport@ events while sound is coming out; the last one reports
-- the final position.
ensureWatcher :: Session -> Emit -> St -> IO St
ensureWatcher s emit st
    | isJust (stWatcher st) = return st
    | otherwise = do
        tid <- forkIO loop
        return st { stWatcher = Just tid }
  where
    loop = do
        threadDelay (sessionTelemetryMicros (sConfig s))
        continue <- modifyMVar (sState s) $ \before -> do
            st' <- noticeLoss before
            active <- case stAudio st' of
                Just b -> (\status heard -> statusPlaying status || heardPlaying heard)
                    <$> backendStatus b SongVoice <*> backendHeard b SongVoice
                Nothing -> return False
            emitTransport emit st'
            return (if active then st' else st' { stWatcher = Nothing }, active)
        when continue loop
