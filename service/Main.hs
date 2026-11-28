{-# LANGUAGE OverloadedStrings #-}
-- | @sonora-service@: the persistent Sonora engine process.
--
-- Speaks the protocol in "Sonora.Service.Protocol" on stdin/stdout. All
-- diagnostics are written to stderr. Exits 0 on end of input or a
-- @shutdown@ request.
module Main (main) where

import Control.Concurrent.MVar
import Control.Exception (finally)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Options.Applicative
import System.Exit (exitSuccess)
import System.IO

import Paths_sonora (version)
import Sonora.Service.Methods
import Sonora.Service.Server
import Sonora.Service.Session (SessionConfig (..), closeSession, defaultSessionConfig, newSession)

data Options = Options
    { optMaxMessageBytes :: !Int
    , optDiagnostics     :: !Bool
    , optQuiet           :: !Bool
    , optRecoveryDir     :: !(Maybe FilePath)
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> option auto (long "max-message-bytes" <> metavar "BYTES" <> value (configMaxMessageBytes defaultServerConfig)
                     <> showDefault <> help "Largest accepted request line")
    <*> switch (long "enable-diagnostic-methods" <> help "Serve diagnostic.* methods used by integration tests")
    <*> switch (long "quiet" <> help "Suppress diagnostic logging on stderr")
    <*> optional (strOption (long "recovery-dir" <> metavar "DIR"
                             <> help "Keep unsaved work here between saves and offer it after a crash"))

main :: IO ()
main = do
    opts <- execParser $ info (optionsParser <**> helper)
        (fullDesc <> progDesc "Sonora engine service (line-delimited JSON on stdin/stdout)")
    hSetEncoding stderr utf8
    hSetBuffering stderr LineBuffering
    logLock <- newMVar ()
    session <- newSession defaultSessionConfig { sessionRecoveryDir = optRecoveryDir opts }
    let logLine msg
            | optQuiet opts = return ()
            | otherwise = withMVar logLock $ \_ -> TIO.hPutStrLn stderr ("[sonora-service] " <> msg)
        methods
            | optDiagnostics opts = applicationMethods session `Map.union` diagnosticMethods
            | otherwise           = applicationMethods session
        cfg = defaultServerConfig
            { configMaxMessageBytes = max 1024 (optMaxMessageBytes opts)
            , configServiceVersion  = T.pack (showVersion version)
            , configMethods         = methods
              -- Session operations change shared state; apply them in order.
            , configSerialMethods   = serialMethods
            , configLog             = logLine
            }
    _ <- runServer cfg stdin stdout `finally` closeSession session
    exitSuccess
