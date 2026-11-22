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
-- finishes during playback it is swapped in at the same position.
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
    , transportView
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, void, when)
import Data.Aeson
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)

import Sonora.Audio.Backend
import Sonora.Audio.Device (openDeviceBackend)
import Sonora.Project.Compile
import Sonora.Project.Render (renderCompiled)
import Sonora.Project.Score (scorePatches)
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Service.Protocol
import Sonora.Signal.Core (Frame)

type Emit = Event -> IO ()

data SessionConfig = SessionConfig
    { sessionSampleRate   :: !Int
      -- | Renders a compiled project, reporting @(completed, total)@ progress.
    , sessionRenderer     :: Int -> (Int -> Int -> IO ()) -> CompiledProject -> IO (V.Vector Frame)
    , sessionOpenAudio    :: Int -> IO (Either AudioError AudioBackend)
    , sessionInitialScore :: !Text
      -- | Interval between @transport@ events while playing, in microseconds.
    , sessionTelemetryMicros :: !Int
    }

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
    { sessionSampleRate      = 44100
    , sessionRenderer        = renderCompiled
    , sessionOpenAudio       = openDeviceBackend
    , sessionInitialScore    = starterScore
    , sessionTelemetryMicros = 100000
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
    }

data Session = Session
    { sConfig :: !SessionConfig
    , sState  :: !(MVar St)
    }

newSession :: SessionConfig -> IO Session
newSession cfg = Session cfg <$> newMVar St
    { stProject  = newProject (sessionInitialScore cfg)
    , stNextJob  = 1
    , stRunning  = Nothing
    , stFinished = Nothing
    , stFailure  = Nothing
    , stAudio    = Nothing
    , stLoaded   = Nothing
    , stWatcher  = Nothing
    }

-- | Stop rendering and playback and release the audio device.
closeSession :: Session -> IO ()
closeSession s = modifyMVar_ (sState s) $ \st -> do
    forM_ (stRunning st) (killThread . jobThread)
    forM_ (stWatcher st) killThread
    forM_ (stAudio st) backendClose
    return st { stRunning = Nothing, stWatcher = Nothing, stAudio = Nothing, stLoaded = Nothing }

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
            , "hasPatchOverride" .= isJust (trackPatchOverride t)
            , "mix"              .= trackMix t
            ]
    return $ object
        [ "revision"       .= projectRevision p
        , "scoreText"      .= projectScoreText p
        , "scoreIsCurrent" .= scoreIsCurrent p
        , "diagnostics"    .= projectDiagnostics p
        , "tracks"         .= map trackView (projectTracks p)
        , "master"         .= projectMaster p
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
    case applyCommand cmd (stProject st) of
        Left (CommandError code msg params) ->
            let ecode = if code == "unknown_track" then UnknownTrack else InvalidValue
                detail = if null params then Nothing else Just (object ["params" .= params])
            in return (st, Left (ServiceError ecode msg detail))
        Right p -> do
            st' <- startRenderLocked s emit False st { stProject = p }
            v <- projectViewLocked st'
            return (st', Right v)

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
                tid <- forkIO (runJob s emit jid rev inputs compiled)
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

runJob :: Session -> Emit -> Int -> Revision -> RenderInputs -> CompiledProject -> IO ()
runJob s emit jid rev inputs compiled = do
    t0 <- getMonotonicTime
    let progress done total = emit $ Event "render.progress" $ object
            ["jobId" .= jid, "revision" .= rev, "completed" .= done, "total" .= total]
    result <- try (sessionRenderer (sConfig s) (sessionSampleRate (sConfig s)) progress compiled)
    t1 <- getMonotonicTime
    case result of
        Left (e :: SomeException) | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
        _ -> modifyMVar_ (sState s) $ \st ->
            if fmap jobId (stRunning st) /= Just jid
                then return st  -- superseded while finishing
                else case result of
                    Right frames -> do
                        let fin = Finished jid rev inputs frames (sessionSampleRate (sConfig s)) (round ((t1 - t0) * 1000))
                            isCurrent = inputs == renderInputs (stProject st)
                        emit (Event "render.finished" (finishedJson fin isCurrent))
                        swapInIfPlaying emit st { stRunning = Nothing, stFinished = Just fin }
                    Left e -> do
                        let msg = T.pack (displayException e)
                        emit $ Event "render.failed" $ object ["jobId" .= jid, "revision" .= rev, "message" .= msg]
                        return st { stRunning = Nothing, stFailure = Just (Failure jid rev inputs msg) }

-- | During playback, replace the playing render with the latest one at the
-- same position.
swapInIfPlaying :: Emit -> St -> IO St
swapInIfPlaying emit st = case (stAudio st, stLoaded st, stFinished st) of
    (Just b, Just loaded, Just fin) | finJobId loaded /= finJobId fin -> do
        status <- backendStatus b SongVoice
        if not (statusPlaying status) then return st else do
            backendLoad b SongVoice (finFrames fin)
            backendSeek b SongVoice (statusPosition status)
            backendSetPlaying b SongVoice True
            let st' = st { stLoaded = Just fin }
            transportViewLocked st' >>= emit . Event "transport"
            return st'
    _ -> return st

-------------------------------------------------------------------------------
-- Transport

transportView :: Session -> IO Value
transportView s = withMVar (sState s) transportViewLocked

transportViewLocked :: St -> IO Value
transportViewLocked st = case (stAudio st, stLoaded st) of
    (Just b, Just fin) -> do
        status <- backendStatus b SongVoice
        lost <- backendDeviceLost b
        let ahead = if statusPlaying status then fromIntegral (backendLatencyFrames b) else 0 :: Word64
            audible = if statusPosition status > ahead then statusPosition status - ahead else 0
        return $ object
            [ "playing"         .= statusPlaying status
            , "positionSeconds" .= (fromIntegral audible / fromIntegral (backendSampleRate b) :: Double)
            , "durationSeconds" .= durationSeconds fin
            , "jobId"           .= finJobId fin
            , "revision"        .= finRevision fin
            , "current"         .= (finInputs fin == renderInputs (stProject st))
            , "deviceLost"      .= lost
            ]
    _ -> return $ object
        [ "playing" .= False, "positionSeconds" .= (0 :: Double)
        , "durationSeconds" .= maybe (0 :: Double) durationSeconds (stFinished st)
        , "jobId" .= (Nothing :: Maybe Int), "revision" .= (Nothing :: Maybe Int)
        , "current" .= False, "deviceLost" .= False ]

-- | Play the latest finished render from the current position.
play :: Session -> Emit -> IO (Either ServiceError Value)
play s emit = modifyMVar (sState s) $ \st -> case stFinished st of
    Nothing -> return (st, Left (serviceError NoRender
        (if isJust (stRunning st) then "The song is still rendering" else "Nothing has been rendered yet")))
    Just fin -> do
        audio <- ensureAudio s st
        case audio of
            Left err -> return (st { stAudio = Nothing, stLoaded = Nothing },
                Left (serviceError AudioUnavailable (T.pack (audioErrorMessage err))))
            Right st1@St { stAudio = Just b } -> do
                st2 <- if fmap finJobId (stLoaded st1) == Just (finJobId fin) then return st1 else do
                    pos <- if isJust (stLoaded st1) then statusPosition <$> backendStatus b SongVoice else return 0
                    backendLoad b SongVoice (finFrames fin)
                    backendSeek b SongVoice pos
                    return st1 { stLoaded = Just fin }
                status <- backendStatus b SongVoice
                when (statusPosition status >= statusLength status) $ backendSeek b SongVoice 0
                backendSetPlaying b SongVoice True
                st3 <- ensureWatcher s emit st2
                v <- transportViewLocked st3
                emit (Event "transport" v)
                return (st3, Right v)
            Right st1 -> return (st1, Left (serviceError AudioUnavailable "Audio output is not open"))

pause :: Session -> Emit -> IO Value
pause s emit = transportOp s emit $ \b -> backendSetPlaying b SongVoice False

stop :: Session -> Emit -> IO Value
stop s emit = transportOp s emit $ \b -> backendSetPlaying b SongVoice False >> backendSeek b SongVoice 0

transportOp :: Session -> Emit -> (AudioBackend -> IO ()) -> IO Value
transportOp s emit op = withMVar (sState s) $ \st -> do
    case (stAudio st, stLoaded st) of
        (Just b, Just _) -> op b
        _ -> return ()
    v <- transportViewLocked st
    emit (Event "transport" v)
    return v

-- | An open, working audio backend; reopens one that lost its device.
ensureAudio :: Session -> St -> IO (Either AudioError St)
ensureAudio s st = do
    usable <- case stAudio st of
        Just b -> not <$> backendDeviceLost b
        Nothing -> return False
    if usable then return (Right st) else do
        forM_ (stAudio st) backendClose
        opened <- sessionOpenAudio (sConfig s) (sessionSampleRate (sConfig s))
        return $ fmap (\b -> st { stAudio = Just b, stLoaded = Nothing }) opened

-- | Emit @transport@ events while playing; stops once playback stops.
ensureWatcher :: Session -> Emit -> St -> IO St
ensureWatcher s emit st
    | isJust (stWatcher st) = return st
    | otherwise = do
        tid <- forkIO loop
        return st { stWatcher = Just tid }
  where
    loop = do
        threadDelay (sessionTelemetryMicros (sConfig s))
        continue <- modifyMVar (sState s) $ \st' -> do
            v <- transportViewLocked st'
            emit (Event "transport" v)
            playing <- case stAudio st' of
                Just b -> do
                    status <- backendStatus b SongVoice
                    lost <- backendDeviceLost b
                    return (statusPlaying status && not lost)
                Nothing -> return False
            return (if playing then (st', True) else (st' { stWatcher = Nothing }, False))
        when continue loop
