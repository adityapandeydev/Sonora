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
    , maxVersions
    , maxPeakBuckets
    , minLoopSeconds
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

import Sonora.Audio.Audition
import Sonora.Audio.Backend
import Sonora.Audio.Device (openDeviceBackend)
import Sonora.DSL.Print (printSong)
import Sonora.Generate.Score
import Sonora.Project.Compile
import Sonora.Project.Layout (outline, songInfo, timing)
import Sonora.Project.Render (renderCompiled)
import Sonora.Project.Score (scorePatches)
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Service.Protocol
import Sonora.Signal.Core (Frame)
import Sonora.Signal.Waveform (waveformPeaks)
import Sonora.Synth.Patch (ParamError (..), Patch, validatePatch)

type Emit = Event -> IO ()

data SessionConfig = SessionConfig
    { sessionSampleRate   :: !Int
      -- | Renders a compiled project, reporting @(completed, total)@ progress.
    , sessionRenderer     :: Int -> (Int -> Int -> IO ()) -> CompiledProject -> IO (V.Vector Frame)
    , sessionOpenAudio    :: Int -> IO (Either AudioError AudioBackend)
    , sessionInitialScore :: !Text
      -- | Interval between @transport@ events while playing, in microseconds.
    , sessionTelemetryMicros :: !Int
      -- | Renders one auditioned note.
    , sessionAuditionRenderer :: Int -> Patch -> AuditionNote -> IO (V.Vector Frame)
    }

defaultSessionConfig :: SessionConfig
defaultSessionConfig = SessionConfig
    { sessionSampleRate      = 44100
    , sessionRenderer        = renderCompiled
    , sessionOpenAudio       = openDeviceBackend
    , sessionInitialScore    = starterScore
    , sessionTelemetryMicros = 40000
    , sessionAuditionRenderer = \sr p n -> evaluate (renderAudition sr p n)
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
newSession cfg = Session cfg <$> newMVar St
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
    }

-- | Stop rendering and playback and release the audio device.
closeSession :: Session -> IO ()
closeSession s = modifyMVar_ (sState s) $ \st -> do
    forM_ (stRunning st) (killThread . jobThread)
    forM_ (stWatcher st) killThread
    forM_ (stAudition st >>= auWorker) killThread
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
applyLocked s emit cmd st = case applyCommand cmd (stProject st) of
    Left (CommandError code msg params) ->
        let ecode = if code == "unknown_track" then UnknownTrack else InvalidValue
            detail = if null params then Nothing else Just (object ["params" .= params])
        in return (Left (ServiceError ecode msg detail))
    Right p -> do
        let changed = fmap validText (projectValidScore p) /= fmap validText (projectValidScore (stProject st))
            since = if changed then projectRevision p else stValidSince st
        Right <$> startRenderLocked s emit False st { stProject = p, stValidSince = since }

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
        replaceScore s emit txt $ \st ->
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
        (v : _) -> replaceScore s emit (versionText v) $ \st ->
            let kept = keepCurrent ("Before restoring " <> T.toLower (T.take 1 (versionLabel v)) <> T.drop 1 (versionLabel v)) st
            in const kept

-- | Set the score text and update the kept versions in one step. @keep@
-- sees the state before the change and the revision after it; only its
-- versions are used.
replaceScore :: Session -> Emit -> Text -> (St -> Revision -> St) -> IO (Either ServiceError Value)
replaceScore s emit txt keep = modifyMVar (sState s) $ \st ->
    applyLocked s emit (SetScoreText txt) st >>= \r -> case r of
        Left e -> return (st, Left e)
        Right st' -> do
            let versions = keep st (projectRevision (stProject st'))
                kept = st' { stVersions = stVersions versions, stNextVersion = stNextVersion versions }
            v <- projectViewLocked kept
            return (kept, Right v)

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
                            , "message" .= T.pack (displayException e) ]
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
                        swapIn emit st { stRunning = Nothing, stFinished = Just fin }
                    Left e -> do
                        let msg = T.pack (displayException e)
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
