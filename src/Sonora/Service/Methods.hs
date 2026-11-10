{-# LANGUAGE OverloadedStrings #-}
-- | Method tables served by @sonora-service@.
module Sonora.Service.Methods
    ( applicationMethods
    , diagnosticMethods
    ) where

import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Sonora.Service.Protocol
import Sonora.Service.Server (Handler, ServerContext (..))

-- | Product methods. Empty until project and render commands are added.
applicationMethods :: Map.Map Text Handler
applicationMethods = Map.empty

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
