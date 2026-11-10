{-# LANGUAGE OverloadedStrings #-}
-- | @sonora-service@: the persistent Sonora engine process.
--
-- Speaks the protocol in "Sonora.Service.Protocol" on stdin/stdout. All
-- diagnostics are written to stderr. Exits 0 on end of input or a
-- @shutdown@ request.
module Main (main) where

import Control.Concurrent.MVar
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

data Options = Options
    { optMaxMessageBytes :: !Int
    , optDiagnostics     :: !Bool
    , optQuiet           :: !Bool
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> option auto (long "max-message-bytes" <> metavar "BYTES" <> value (configMaxMessageBytes defaultServerConfig)
                     <> showDefault <> help "Largest accepted request line")
    <*> switch (long "enable-diagnostic-methods" <> help "Serve diagnostic.* methods used by integration tests")
    <*> switch (long "quiet" <> help "Suppress diagnostic logging on stderr")

main :: IO ()
main = do
    opts <- execParser $ info (optionsParser <**> helper)
        (fullDesc <> progDesc "Sonora engine service (line-delimited JSON on stdin/stdout)")
    hSetEncoding stderr utf8
    hSetBuffering stderr LineBuffering
    logLock <- newMVar ()
    let logLine msg
            | optQuiet opts = return ()
            | otherwise = withMVar logLock $ \_ -> TIO.hPutStrLn stderr ("[sonora-service] " <> msg)
        methods
            | optDiagnostics opts = applicationMethods `Map.union` diagnosticMethods
            | otherwise           = applicationMethods
        cfg = defaultServerConfig
            { configMaxMessageBytes = max 1024 (optMaxMessageBytes opts)
            , configServiceVersion  = T.pack (showVersion version)
            , configMethods         = methods
            , configLog             = logLine
            }
    _ <- runServer cfg stdin stdout
    exitSuccess
