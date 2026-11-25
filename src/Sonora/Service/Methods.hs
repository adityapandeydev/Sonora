{-# LANGUAGE OverloadedStrings #-}
-- | Method tables served by @sonora-service@.
module Sonora.Service.Methods
    ( applicationMethods
    , serialMethods
    , diagnosticMethods
    ) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Sonora.Service.Protocol
import Sonora.Service.Server (Handler, ServerContext (..))
import Sonora.Service.Session

-- | Product methods, served from one session.
--
-- * @project.get@ → project view.
-- * @project.setScore@ @{text}@ → project view; starts a render when the
--   playable song changed.
-- * @project.apply@ @{command}@ → project view (see "Sonora.Project.Types"
--   for command objects); errors: @unknown_track@, @invalid_value@.
-- * @render.start@, @render.cancel@ → render status.
-- * @transport.play@ (errors: @no_render@, @audio_unavailable@),
--   @transport.pause@, @transport.stop@, @transport.state@,
--   @transport.seek@ @{seconds}@, @transport.setLoop@ @{loop: {start, end} | null}@,
--   @transport.setVolume@ @{volume}@ → transport status; out-of-range values
--   are @invalid_value@.
-- * @render.peaks@ @{buckets}@ → waveform overview of the latest render.
-- * @project.timing@ → when each line of the last valid score sounds.
-- * @project.generate@ @{settings: {seed, root, scale, bpm, bars}}@ → project
--   view; replaces the score with a generated one, keeping the replaced text
--   and the result in @versions@. Invalid settings are @invalid_value@ with
--   the parameters at fault.
-- * @project.restoreVersion@ @{id}@ → project view; brings back a kept score,
--   keeping the text it replaces.
--
-- Events: @render.started@, @render.progress@, @render.finished@,
-- @render.failed@, @render.cancelled@ (all carry @jobId@ and @revision@),
-- and @transport@ (on changes, and periodically while playing).
applicationMethods :: Session -> Map.Map Text Handler
applicationMethods s = Map.fromList
    [ ("project.get", \_ _ -> Right <$> projectView s)
    , ("project.setScore", \ctx params -> case parseParams (withObject "setScore" (.: "text")) params of
        Left e -> return (Left e)
        Right txt -> Right <$> setScore s (contextEmit ctx) txt)
    , ("project.apply", \ctx params -> case parseParams (withObject "apply" (.: "command")) params of
        Left e -> return (Left e)
        Right cmd -> applyProjectCommand s (contextEmit ctx) cmd)
    , ("render.start", \ctx _ -> Right <$> startRender s (contextEmit ctx))
    , ("render.cancel", \ctx _ -> Right <$> cancelRender s (contextEmit ctx))
    , ("transport.play", \ctx _ -> play s (contextEmit ctx))
    , ("transport.pause", \ctx _ -> Right <$> pause s (contextEmit ctx))
    , ("transport.stop", \ctx _ -> Right <$> stop s (contextEmit ctx))
    , ("transport.state", \_ _ -> Right <$> transportView s)
    , ("transport.seek", \ctx params -> withParams (.: "seconds") params (seek s (contextEmit ctx)))
    , ("transport.setLoop", \ctx params -> withParams loopParam params (setLoop s (contextEmit ctx)))
    , ("transport.setVolume", \ctx params -> withParams (.: "volume") params (setVolume s (contextEmit ctx)))
    , ("render.peaks", \_ params -> withParams (.: "buckets") params (renderPeaks s))
    , ("project.timing", \_ _ -> Right <$> scoreTiming s)
    , ("project.generate", \ctx params -> withParams (.: "settings") params (generate s (contextEmit ctx)))
    , ("project.restoreVersion", \ctx params -> withParams (.: "id") params (restoreVersion s (contextEmit ctx)))
    ]

withParams :: (Object -> Parser a) -> Value -> (a -> IO (Either ServiceError Value)) -> IO (Either ServiceError Value)
withParams parser params act = either (return . Left) act (parseParams (withObject "params" parser) params)

-- | The loop must be present: an object to set it, null to clear it.
loopParam :: Object -> Parser (Maybe (Double, Double))
loopParam o = do
    v <- o .: "loop"
    case v of
        Null -> return Nothing
        _ -> Just <$> withObject "loop" (\l -> (,) <$> l .: "start" <*> l .: "end") v

-- | Application methods that must run in arrival order: everything that
-- reads or changes shared state. Overviews and timing only read a snapshot.
serialMethods :: Set.Set Text
serialMethods = Set.fromList
    [ "project.get", "project.setScore", "project.apply", "render.start", "render.cancel"
    , "transport.play", "transport.pause", "transport.stop", "transport.state"
    , "transport.seek", "transport.setLoop", "transport.setVolume"
    , "project.generate", "project.restoreVersion" ]

-- | Methods for exercising the protocol from tests and host integration
-- checks. Only served when the service is started with
-- @--enable-diagnostic-methods@.
diagnosticMethods :: Map.Map Text Handler
diagnosticMethods = Map.fromList
    [ ("diagnostic.echo", \_ params -> return (Right params))
    , ("diagnostic.sleep", \_ params ->
        case parseParams (withObject "sleep" (.: "ms")) params of
            Left e -> return (Left e)
            Right ms
                | ms < 0 || ms > (60000 :: Int) -> return (Left (serviceError InvalidParams "\"ms\" must be between 0 and 60000"))
                | otherwise -> do
                    threadDelay (ms * 1000)
                    return (Right (object ["slept" .= ms])))
    , ("diagnostic.fail", \_ _ -> ioError (userError "diagnostic failure"))
    , ("diagnostic.event", \ctx params -> do
        contextEmit ctx (Event "diagnostic" params)
        return (Right (object [])))
    ]
