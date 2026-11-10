{-# LANGUAGE OverloadedStrings #-}
module ServiceSpec (serviceTests) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM, replicateM)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO
import System.Process
import System.Timeout (timeout)

import Sonora.Service.Methods (diagnosticMethods)
import Sonora.Service.Protocol
import Sonora.Service.Server

-------------------------------------------------------------------------------
-- Message helpers

request :: Text -> Text -> Value -> BS.ByteString
request rid m params =
    BL.toStrict (encode (object ["v" .= protocolVersion, "id" .= rid, "method" .= m, "params" .= params])) <> "\n"

hello :: BS.ByteString
hello = request "hello" "hello" (object ["protocols" .= [protocolVersion]])

path :: [Text] -> Value -> Maybe Value
path [] v = Just v
path (k : ks) (Object o) = KM.lookup (Key.fromText k) o >>= path ks
path _ _ = Nothing

idOf :: Value -> Maybe Value
idOf = path ["id"]

isOk :: Text -> Value -> Bool
isOk rid v = idOf v == Just (String rid) && path ["ok"] v == Just (Bool True) && path ["v"] v == Just (Number 1)

isErr :: Maybe Text -> Text -> Value -> Bool
isErr rid code v =
    idOf v == Just (maybe Null String rid)
        && path ["ok"] v == Just (Bool False)
        && path ["error", "code"] v == Just (String code)
        && isJust (path ["error", "message"] v)

-------------------------------------------------------------------------------
-- In-process sessions over OS pipes

data Session = Session
    { send :: BS.ByteString -> IO ()
    , recv :: IO (Maybe Value)       -- ^ Next message, or Nothing after 3 s.
    , closeInput :: IO ()
    , exitReason :: IO (Maybe ExitReason)
    }

withSession :: Int -> (Session -> IO Bool) -> IO Bool
withSession maxBytes act = do
    (inR, inW) <- createPipe
    (outR, outW) <- createPipe
    mapM_ (`hSetBinaryMode` True) [inW, outR]
    done <- newEmptyMVar :: IO (MVar (Either SomeException ExitReason))
    let cfg = defaultServerConfig { configMaxMessageBytes = maxBytes, configMethods = diagnosticMethods }
    _ <- forkIO (try (runServer cfg inR outW) >>= putMVar done)
    let sess = Session
            { send = \b -> BS.hPut inW b >> hFlush inW
            , recv = do
                line <- timeout 3000000 (BC.hGetLine outR)
                return (line >>= decodeStrict)
            , closeInput = hClose inW
            , exitReason = do
                r <- timeout 3000000 (readMVar done)
                return $ case r of
                    Just (Right reason) -> Just reason
                    _ -> Nothing :: Maybe ExitReason
            }
        quietly = handle (\e -> const (return ()) (e :: IOException))
    act sess `finally` mapM_ (quietly . hClose) [inW, inR, outW, outR]

recvN :: Session -> Int -> IO [Value]
recvN s n = fmap (maybe [] id . sequence) (replicateM n (recv s))

-------------------------------------------------------------------------------

check :: String -> IO Bool -> IO Bool
check name t = do
    ok <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if ok then "PASSED" else "FAILED")
    return ok

session :: (Session -> IO Bool) -> IO Bool
session = withSession 4096

inProcessTests :: [(String, IO Bool)]
inProcessTests =
    [ ("Handshake negotiates version and lists methods", session $ \s -> do
        send s hello
        r <- recv s
        return $ case r of
            Just v -> isOk "hello" v
                && path ["result", "protocol"] v == Just (Number 1)
                && path ["result", "service"] v == Just (String "sonora-service")
                && isJust (path ["result", "limits", "maxMessageBytes"] v)
                && maybe False (\m -> String "ping" `elem` asList m && String "diagnostic.echo" `elem` asList m)
                         (path ["result", "methods"] v)
            Nothing -> False)

    , ("Requests before hello are rejected", session $ \s -> do
        send s (request "p" "ping" (object []))
        send s hello
        rs <- recvN s 2
        return $ case rs of
            [a, b] -> isErr (Just "p") "handshake_required" a && isOk "hello" b
            _ -> False)

    , ("No common protocol version", session $ \s -> do
        send s (request "h" "hello" (object ["protocols" .= [99 :: Int]]))
        r <- recv s
        return (maybe False (\v -> isErr (Just "h") "unsupported_version" v
                                  && path ["error", "data", "supported"] v == Just (toJSON [protocolVersion])) r))

    , ("Envelope version mismatch keeps the request id", session $ \s -> do
        send s "{\"v\":2,\"id\":\"x\",\"method\":\"ping\"}\n"
        r <- recv s
        return (maybe False (isErr (Just "x") "unsupported_version") r))

    , ("Malformed messages get structured errors", session $ \s -> do
        send s hello
        send s "{not json\n"
        send s "[1,2]\n"
        send s "{\"v\":1,\"method\":\"ping\"}\n"
        send s "{\"v\":1,\"id\":\"q\",\"method\":\"ping\",\"params\":[1]}\n"
        send s "{\"v\":1,\"id\":\"\",\"method\":\"ping\"}\n"
        rs <- recvN s 6
        return $ case rs of
            [h, a, b, c, d, e] -> isOk "hello" h
                && isErr Nothing "parse_error" a
                && isErr Nothing "invalid_request" b
                && isErr Nothing "invalid_request" c
                && isErr (Just "q") "invalid_request" d
                && isErr Nothing "invalid_request" e
            _ -> False)

    , ("Second hello and unknown methods are rejected", session $ \s -> do
        send s hello
        send s (request "h2" "hello" (object ["protocols" .= [1 :: Int]]))
        send s (request "u" "no.such.method" (object []))
        rs <- recvN s 3
        return $ case rs of
            [_, a, b] -> isErr (Just "h2") "already_initialized" a && isErr (Just "u") "method_not_found" b
            _ -> False)

    , ("Pipelined messages in one write, CRLF and blank lines", session $ \s -> do
        send s (BC.concat [ BC.init hello, "\r\n", "\n   \n"
                          , request "a" "ping" (object [])
                          , request "b" "diagnostic.echo" (object ["x" .= (1 :: Int)]) ])
        rs <- recvN s 3
        return $ case rs of
            [h, a, b] -> isOk "hello" h && isOk "a" a && isOk "b" b
                && path ["result", "x"] b == Just (Number 1)
            _ -> False)

    , ("Oversized line is skipped and the stream recovers", session $ \s -> do
        send s hello
        send s (BC.replicate 200000 'a' <> "\n")
        send s (request "after" "ping" (object []))
        rs <- recvN s 3
        return $ case rs of
            [_, big, p] -> isErr Nothing "message_too_large" big && isOk "after" p
            _ -> False)

    , ("Slow requests do not block later ones", session $ \s -> do
        send s hello
        send s (request "slow" "diagnostic.sleep" (object ["ms" .= (400 :: Int)]))
        send s (request "fast" "ping" (object []))
        rs <- recvN s 3
        return $ case rs of
            [_, a, b] -> isOk "fast" a && isOk "slow" b && path ["result", "slept"] b == Just (Number 400)
            _ -> False)

    , ("Cancel answers the target once and suppresses its result", session $ \s -> do
        send s hello
        send s (request "job" "diagnostic.sleep" (object ["ms" .= (300 :: Int)]))
        send s (request "c" "cancel" (object ["target" .= ("job" :: Text)]))
        rs <- recvN s 3
        threadDelay 600000
        send s (request "c2" "cancel" (object ["target" .= ("job" :: Text)]))
        later <- recv s
        return $ case (rs, later) of
            ([_, a, b], Just c) ->
                isErr (Just "job") "cancelled" a
                    && isOk "c" b && path ["result", "cancelled"] b == Just (Bool True)
                    && isOk "c2" c && path ["result", "cancelled"] c == Just (Bool False)
            _ -> False)

    , ("Duplicate in-flight ids are rejected", session $ \s -> do
        send s hello
        send s (request "d" "diagnostic.sleep" (object ["ms" .= (200 :: Int)]))
        send s (request "d" "diagnostic.echo" (object []))
        rs <- recvN s 3
        return $ case rs of
            [_, a, b] -> isErr (Just "d") "duplicate_id" a && isOk "d" b
            _ -> False)

    , ("Handler failures and bad params are reported; service continues", session $ \s -> do
        send s hello
        send s (request "f" "diagnostic.fail" (object []))
        send s (request "bp" "diagnostic.sleep" (object ["ms" .= ("soon" :: Text)]))
        send s (request "c" "cancel" (object []))
        send s (request "p" "ping" (object []))
        rs <- recvN s 5
        let byId rid = filter (\v -> idOf v == Just (String rid)) rs
        return $ length rs == 5
            && all (isErr (Just "f") "internal_error") (byId "f") && length (byId "f") == 1
            && all (isErr (Just "bp") "invalid_params") (byId "bp") && length (byId "bp") == 1
            && all (isErr (Just "c") "invalid_params") (byId "c") && length (byId "c") == 1
            && length (byId "p") == 1)

    , ("Events are delivered as event envelopes", session $ \s -> do
        send s hello
        send s (request "e" "diagnostic.event" (object ["n" .= (7 :: Int)]))
        rs <- recvN s 3
        return $ case rs of
            [_, ev, r] -> path ["event"] ev == Just (String "diagnostic")
                && path ["data", "n"] ev == Just (Number 7)
                && path ["v"] ev == Just (Number 1)
                && isOk "e" r
            _ -> False)

    , ("Shutdown stops in-flight work and returns", session $ \s -> do
        send s hello
        send s (request "long" "diagnostic.sleep" (object ["ms" .= (5000 :: Int)]))
        send s (request "bye" "shutdown" (object []))
        rs <- recvN s 3
        reason <- exitReason s
        return $ case rs of
            [_, a, b] -> isErr (Just "long") "shutting_down" a && isOk "bye" b
                && reason == Just ShutdownRequested
            _ -> False)

    , ("End of input stops the service", session $ \s -> do
        send s hello
        _ <- recv s
        send s (request "long" "diagnostic.sleep" (object ["ms" .= (5000 :: Int)]))
        threadDelay 50000
        closeInput s
        r <- recv s
        reason <- exitReason s
        return (maybe False (isErr (Just "long") "shutting_down") r && reason == Just EndOfInput))

    , ("Final unterminated line is still processed", session $ \s -> do
        send s (BC.init hello)
        closeInput s
        r <- recv s
        reason <- exitReason s
        return (maybe False (isOk "hello") r && reason == Just EndOfInput))
    ]
  where
    asList (Array a) = foldr (:) [] a
    asList _ = []

-------------------------------------------------------------------------------
-- The real executable

findService :: IO (Maybe FilePath)
findService = do
    env <- lookupEnv "SONORA_SERVICE_EXE"
    maybe (findExecutable "sonora-service") (return . Just) env

data Proc = Proc { pIn :: Handle, pOut :: Handle, pErr :: Handle, pHandle :: ProcessHandle }

withServiceProcess :: FilePath -> [String] -> (Proc -> IO Bool) -> IO Bool
withServiceProcess exe args act = do
    (Just i, Just o, Just e, ph) <- createProcess (proc exe args)
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
    mapM_ (`hSetBinaryMode` True) [i, o]
    act (Proc i o e ph) `finally` (terminateProcess ph >> mapM_ (\h -> hClose h `catch` ignoreIO) [i, o, e])
  where
    ignoreIO :: IOException -> IO ()
    ignoreIO _ = return ()

-- | Wait for exit, at most 5 seconds.
waitExit :: ProcessHandle -> IO (Maybe ExitCode)
waitExit ph = timeout 5000000 (waitForProcess ph)

processTests :: FilePath -> [(String, IO Bool)]
processTests exe =
    [ ("Service process: stdout carries only protocol messages; exits 0 on EOF", withServiceProcess exe [] $ \p -> do
        BS.hPut (pIn p) (hello <> request "p" "ping" (object []) <> request "d" "diagnostic.echo" (object []))
        hFlush (pIn p)
        hClose (pIn p)
        code <- waitExit (pHandle p)
        out <- BS.hGetContents (pOut p)
        errText <- BS.hGetContents (pErr p)
        let msgs = map decodeStrict (BC.lines out) :: [Maybe Value]
        return $ code == Just ExitSuccess
            && length msgs == 3 && all isJust msgs
            && case sequence msgs of
                Just [h, a, b] -> isOk "hello" h && isOk "p" a
                    -- diagnostic methods are off unless explicitly enabled
                    && isErr (Just "d") "method_not_found" b
                _ -> False
            && "[sonora-service]" `BS.isInfixOf` errText)

    , ("Service process: shutdown request exits 0 with input still open", withServiceProcess exe ["--enable-diagnostic-methods"] $ \p -> do
        BS.hPut (pIn p) (hello <> request "long" "diagnostic.sleep" (object ["ms" .= (30000 :: Int)])
                         <> request "bye" "shutdown" (object []))
        hFlush (pIn p)
        code <- waitExit (pHandle p)
        out <- BS.hGetContents (pOut p)
        let msgs = map decodeStrict (BC.lines out) :: [Maybe Value]
        return $ code == Just ExitSuccess && case sequence msgs of
            Just [_, a, b] -> isErr (Just "long") "shutting_down" a && isOk "bye" b
            _ -> False)

    , ("Service process: --quiet suppresses stderr", withServiceProcess exe ["--quiet"] $ \p -> do
        hClose (pIn p)
        code <- waitExit (pHandle p)
        errText <- BS.hGetContents (pErr p)
        return (code == Just ExitSuccess && BS.null errText))
    ]

serviceTests :: IO Bool
serviceTests = do
    putStrLn "--- Running Service Protocol Tests ---"
    rs <- forM inProcessTests (uncurry check)
    mexe <- findService
    ps <- case mexe of
        Nothing -> do
            putStrLn "  Service process tests: FAILED (sonora-service not found; set SONORA_SERVICE_EXE)"
            return [False]
        Just exe -> forM (processTests exe) (uncurry check)
    return (and rs && and ps)
