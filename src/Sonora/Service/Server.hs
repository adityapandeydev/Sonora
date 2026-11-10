{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The Sonora service loop: reads requests from one handle and writes
-- responses and events to another, using "Sonora.Service.Protocol".
--
-- * The first request must be @hello@ (the handshake).
-- * @ping@, @cancel@ and @shutdown@ are built in and answered in order.
-- * Every other method runs on its own thread, so slow work never blocks
--   reading; @cancel@ stops an in-flight request, which then receives a
--   @cancelled@ error instead of its result. Exactly one response is ever
--   written per accepted request.
-- * Lines longer than the configured limit are discarded without being
--   buffered in full and answered with @message_too_large@.
-- * End of input or @shutdown@ stops all in-flight work (answered with
--   @shutting_down@) and returns.
--
-- Nothing but protocol messages is written to the output handle;
-- diagnostics go through 'configLog'.
module Sonora.Service.Server
    ( ServerConfig (..)
    , defaultServerConfig
    , ServerContext (..)
    , Handler
    , ExitReason (..)
    , runServer
      -- * Input framing
    , InputLine (..)
    , newLineReader
    ) where

import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO

import Sonora.Service.Protocol

-- | What handlers may do besides returning a result.
data ServerContext = ServerContext
    { contextEmit :: Event -> IO ()
    , contextLog  :: Text -> IO ()
    }

-- | A method implementation. It runs on its own thread and may be killed
-- by @cancel@ or shutdown at any point.
type Handler = ServerContext -> Value -> IO (Either ServiceError Value)

data ServerConfig = ServerConfig
    { configMaxMessageBytes :: !Int
    , configServiceName     :: !Text
    , configServiceVersion  :: !Text
    , configMethods         :: !(Map.Map Text Handler)
    , configLog             :: Text -> IO ()
    }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { configMaxMessageBytes = 4 * 1024 * 1024
    , configServiceName     = "sonora-service"
    , configServiceVersion  = "0.0.0"
    , configMethods         = Map.empty
    , configLog             = \_ -> return ()
    }

data ExitReason
    = EndOfInput
    | ShutdownRequested
    deriving (Eq, Show)

builtinMethods :: [Text]
builtinMethods = ["hello", "ping", "cancel", "shutdown"]

-- | Serve until end of input or a @shutdown@ request.
runServer :: ServerConfig -> Handle -> Handle -> IO ExitReason
runServer cfg input output = do
    hSetBinaryMode input True
    hSetBinaryMode output True
    hSetBuffering output (BlockBuffering Nothing)
    nextLine <- newLineReader input (configMaxMessageBytes cfg)
    writeLock <- newMVar ()
    inflight <- newMVar (Map.empty :: Map.Map RequestId ThreadId)
    initialized <- newIORef False

    let logMsg = configLog cfg

        writeLine :: BL.ByteString -> IO ()
        writeLine bytes = withMVar writeLock $ \_ ->
            (BL.hPut output bytes >> BC.hPut output "\n" >> hFlush output)
                `catch` \(e :: IOException) -> logMsg ("output write failed: " <> T.pack (show e))

        respond rid result = writeLine (encodeResponse (Response rid result))
        failure rid code msg = respond rid (Left (serviceError code msg))

        ctx = ServerContext
            { contextEmit = writeLine . encodeEvent
            , contextLog  = logMsg
            }

        -- Stop all in-flight requests, answering each one.
        stopAll code msg = do
            running <- modifyMVar inflight (\m -> return (Map.empty, Map.toList m))
            forM_ running $ \(rid, tid) -> do
                void (forkIO (killThread tid))
                failure (Just rid) code msg
            return (length running)

        spawn req handler = do
            let rid = requestId req
            modifyMVar_ inflight $ \m -> do
                -- Registered before the worker can finish: it must take
                -- this lock to claim the right to respond.
                tid <- forkIO (worker rid handler (requestParams req))
                return (Map.insert rid tid m)

        worker rid handler params = do
            outcome <- try $ do
                result <- handler ctx params
                evaluate (BL.toStrict (encodeResponse (Response (Just rid) result)))
            encoded <- case outcome of
                Right bytes -> return bytes
                Left (e :: SomeException)
                    | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
                    | otherwise -> do
                        logMsg ("request " <> rid <> " failed: " <> T.pack (displayException e))
                        return $ BL.toStrict $ encodeResponse $ Response (Just rid) $
                            Left (serviceError InternalError (T.pack (displayException e)))
            owned <- modifyMVar inflight (\m -> return (Map.delete rid m, Map.member rid m))
            when owned (writeLine (BL.fromStrict encoded))

        handshake req = do
            done <- readIORef initialized
            if done
                then failure (Just (requestId req)) AlreadyInitialized "Handshake already completed"
                else case parseParams (withObject "hello" (.: "protocols")) (requestParams req) of
                    Left e -> respond (Just (requestId req)) (Left e)
                    Right (offered :: [Int]) ->
                        case filter (`elem` offered) (reverse supportedVersions) of
                            [] -> respond (Just (requestId req)) $ Left $ ServiceError UnsupportedVersion
                                "No mutually supported protocol version"
                                (Just (object ["supported" .= supportedVersions]))
                            (v : _) -> do
                                writeIORef initialized True
                                logMsg ("handshake complete, protocol " <> T.pack (show v))
                                respond (Just (requestId req)) $ Right $ object
                                    [ "protocol"       .= v
                                    , "service"        .= configServiceName cfg
                                    , "serviceVersion" .= configServiceVersion cfg
                                    , "methods"        .= (builtinMethods ++ Map.keys (configMethods cfg))
                                    , "limits"         .= object ["maxMessageBytes" .= configMaxMessageBytes cfg]
                                    ]

        cancel req =
            case parseParams (withObject "cancel" (.: "target")) (requestParams req) of
                Left e -> respond (Just (requestId req)) (Left e)
                Right (target :: RequestId) -> do
                    found <- modifyMVar inflight (\m -> return (Map.delete target m, Map.lookup target m))
                    case found of
                        Just tid -> do
                            void (forkIO (killThread tid))
                            failure (Just target) Cancelled "Request was cancelled"
                        Nothing -> return ()
                    respond (Just (requestId req)) (Right (object ["cancelled" .= maybe False (const True) found]))

        dispatch req = do
            let rid = requestId req
            ready <- readIORef initialized
            busy <- Map.member rid <$> readMVar inflight
            case requestMethod req of
                "hello" -> handshake req >> return Nothing
                _ | not ready -> do
                        failure (Just rid) HandshakeRequired "Send \"hello\" before other requests"
                        return Nothing
                  | busy -> do
                        failure (Just rid) DuplicateId "Another in-flight request uses this id"
                        return Nothing
                "ping" -> respond (Just rid) (Right (object [])) >> return Nothing
                "cancel" -> cancel req >> return Nothing
                "shutdown" -> do
                    n <- stopAll ShuttingDown "Service is shutting down"
                    logMsg ("shutdown requested; stopped " <> T.pack (show n) <> " in-flight request(s)")
                    respond (Just rid) (Right (object []))
                    return (Just ShutdownRequested)
                m -> case Map.lookup m (configMethods cfg) of
                    Just handler -> spawn req handler >> return Nothing
                    Nothing -> do
                        failure (Just rid) MethodNotFound ("Unknown method: " <> m)
                        return Nothing

        loop = do
            line <- nextLine
            case line of
                InputClosed -> do
                    n <- stopAll ShuttingDown "Input closed"
                    logMsg ("input closed; stopped " <> T.pack (show n) <> " in-flight request(s)")
                    return EndOfInput
                Oversized -> do
                    failure Nothing MessageTooLarge
                        ("Message exceeds " <> T.pack (show (configMaxMessageBytes cfg)) <> " bytes")
                    loop
                Line bytes
                    | BS.all isSpaceByte bytes -> loop
                    | otherwise -> case decodeRequest bytes of
                        Left (rid, err) -> respond rid (Left err) >> loop
                        Right req -> dispatch req >>= maybe loop return

    logMsg "service started"
    reason <- loop
    logMsg ("service stopping: " <> T.pack (show reason))
    return reason
  where
    isSpaceByte b = b == 32 || b == 9 || b == 13

data InputLine
    = Line !BS.ByteString  -- ^ One message, without its line terminator.
    | Oversized            -- ^ A line over the limit was skipped.
    | InputClosed            -- ^ Input closed.
    deriving (Eq, Show)

-- | Read newline-terminated messages of at most the given size. Oversized
-- lines are skipped chunk by chunk rather than accumulated. A final
-- unterminated line is returned before end of input.
newLineReader :: Handle -> Int -> IO (IO InputLine)
newLineReader h maxBytes = do
    pending <- newIORef BS.empty
    let chunkSize = 64 * 1024

        finish bytes
            | BS.length line > maxBytes = Oversized
            | otherwise = Line line
          where
            line = if not (BS.null bytes) && BC.last bytes == '\r' then BS.init bytes else bytes

        next = do
            buf <- readIORef pending
            case BC.elemIndex '\n' buf of
                Just i -> do
                    writeIORef pending (BS.drop (i + 1) buf)
                    return (finish (BS.take i buf))
                Nothing -> do
                    writeIORef pending BS.empty
                    if BS.length buf > maxBytes + 1 then skip else accumulate [buf] (BS.length buf)

        accumulate acc len = do
            chunk <- BS.hGetSome h chunkSize
            if BS.null chunk
                then return $ if len == 0 then InputClosed else finish (BS.concat (reverse acc))
                else case BC.elemIndex '\n' chunk of
                    Just i -> do
                        writeIORef pending (BS.drop (i + 1) chunk)
                        return (finish (BS.concat (reverse (BS.take i chunk : acc))))
                    Nothing
                        | len + BS.length chunk > maxBytes + 1 -> skip
                        | otherwise -> accumulate (chunk : acc) (len + BS.length chunk)

        -- Discard input up to and including the next newline.
        skip = do
            chunk <- BS.hGetSome h chunkSize
            if BS.null chunk
                then return Oversized
                else case BC.elemIndex '\n' chunk of
                    Just i  -> writeIORef pending (BS.drop (i + 1) chunk) >> return Oversized
                    Nothing -> skip
    unless (maxBytes > 0) (ioError (userError "newLineReader: limit must be positive"))
    return next
