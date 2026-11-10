{-# LANGUAGE OverloadedStrings #-}
-- | Wire protocol for the Sonora service.
--
-- Messages are UTF-8 JSON objects, one per line, exchanged over the
-- service's stdin (requests) and stdout (responses and events). Every
-- envelope carries the protocol version in @"v"@.
--
-- > {"v":1,"id":"7","method":"ping","params":{}}
-- > {"v":1,"id":"7","ok":true,"result":{}}
-- > {"v":1,"id":"7","ok":false,"error":{"code":"method_not_found","message":"..."}}
-- > {"v":1,"event":"log","data":{...}}
--
-- Responses to messages that could not be attributed to a request (for
-- example malformed JSON) carry @"id":null@.
module Sonora.Service.Protocol
    ( protocolVersion
    , supportedVersions
    , RequestId
    , maxRequestIdLength
    , Request (..)
    , ErrorCode (..)
    , errorCodeText
    , ServiceError (..)
    , serviceError
    , Response (..)
    , Event (..)
    , decodeRequest
    , encodeResponse
    , encodeEvent
    , parseParams
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T

-- | The protocol version spoken by this build.
protocolVersion :: Int
protocolVersion = 1

-- | Versions this build can negotiate during the handshake.
supportedVersions :: [Int]
supportedVersions = [protocolVersion]

-- | Client-chosen request identifier, unique among in-flight requests.
type RequestId = Text

maxRequestIdLength :: Int
maxRequestIdLength = 128

data Request = Request
    { requestId     :: !RequestId
    , requestMethod :: !Text
    , requestParams :: !Value
    } deriving (Eq, Show)

data ErrorCode
    = ParseError          -- ^ Not valid JSON.
    | InvalidRequest      -- ^ JSON, but not a well-formed request envelope.
    | UnsupportedVersion  -- ^ Envelope or handshake version not supported.
    | HandshakeRequired   -- ^ A request other than @hello@ arrived first.
    | AlreadyInitialized  -- ^ A second @hello@.
    | MethodNotFound
    | InvalidParams
    | DuplicateId         -- ^ The id is already used by an in-flight request.
    | MessageTooLarge
    | Cancelled
    | ShuttingDown
    | InternalError
    deriving (Eq, Show, Enum, Bounded)

errorCodeText :: ErrorCode -> Text
errorCodeText c = case c of
    ParseError         -> "parse_error"
    InvalidRequest     -> "invalid_request"
    UnsupportedVersion -> "unsupported_version"
    HandshakeRequired  -> "handshake_required"
    AlreadyInitialized -> "already_initialized"
    MethodNotFound     -> "method_not_found"
    InvalidParams      -> "invalid_params"
    DuplicateId        -> "duplicate_id"
    MessageTooLarge    -> "message_too_large"
    Cancelled          -> "cancelled"
    ShuttingDown       -> "shutting_down"
    InternalError      -> "internal_error"

-- | A structured failure reported to the client.
data ServiceError = ServiceError
    { errorCode    :: !ErrorCode
    , errorMessage :: !Text
    , errorData    :: !(Maybe Value)  -- ^ Optional machine-readable detail.
    } deriving (Eq, Show)

serviceError :: ErrorCode -> Text -> ServiceError
serviceError c m = ServiceError c m Nothing

instance ToJSON ServiceError where
    toJSON (ServiceError c m d) = object $
        [ "code" .= errorCodeText c, "message" .= m ] ++ maybe [] (\v -> ["data" .= v]) d

data Response = Response
    { responseId     :: !(Maybe RequestId)
    , responseResult :: !(Either ServiceError Value)
    } deriving (Eq, Show)

instance ToJSON Response where
    toJSON (Response rid res) = object $
        [ "v" .= protocolVersion, "id" .= rid ] ++ case res of
            Right v -> [ "ok" .= True, "result" .= v ]
            Left e  -> [ "ok" .= False, "error" .= e ]

-- | Unsolicited message from the service.
data Event = Event
    { eventName :: !Text
    , eventData :: !Value
    } deriving (Eq, Show)

instance ToJSON Event where
    toJSON (Event n d) = object [ "v" .= protocolVersion, "event" .= n, "data" .= d ]

-- | Decode one line. On failure, returns the request id when it could be
-- recovered so the error can be attributed.
decodeRequest :: BS.ByteString -> Either (Maybe RequestId, ServiceError) Request
decodeRequest bytes =
    case eitherDecodeStrict bytes of
        Left err -> Left (Nothing, serviceError ParseError (T.pack err))
        Right (Object o) -> do
            let rid = case KM.lookup "id" o of
                    Just (String t) | validId t -> Just t
                    _                           -> Nothing
                failWith c m = Left (rid, serviceError c m)
            case KM.lookup "v" o of
                Just (Number n) | n == fromIntegral protocolVersion -> return ()
                Just (Number _) -> failWith UnsupportedVersion
                    ("Envelope version not supported; this service speaks " <> showT protocolVersion)
                _ -> failWith InvalidRequest "Missing numeric \"v\" field"
            reqId <- case KM.lookup "id" o of
                Just (String t)
                    | validId t -> return t
                    | otherwise -> failWith InvalidRequest
                        ("\"id\" must be a non-empty string of at most " <> showT maxRequestIdLength <> " characters")
                _ -> failWith InvalidRequest "Missing string \"id\" field"
            method <- case KM.lookup "method" o of
                Just (String m) | not (T.null m) -> return m
                _ -> failWith InvalidRequest "Missing string \"method\" field"
            params <- case KM.lookup "params" o of
                Nothing          -> return (Object mempty)
                Just Null        -> return (Object mempty)
                Just p@(Object _) -> return p
                Just _           -> failWith InvalidRequest "\"params\" must be an object"
            return (Request reqId method params)
        Right _ -> Left (Nothing, serviceError InvalidRequest "Message must be a JSON object")
  where
    validId t = not (T.null t) && T.length t <= maxRequestIdLength

encodeResponse :: Response -> BL.ByteString
encodeResponse = encode

encodeEvent :: Event -> BL.ByteString
encodeEvent = encode

-- | Run a params parser, mapping failures to 'InvalidParams'.
parseParams :: (Value -> Parser a) -> Value -> Either ServiceError a
parseParams p v = either (Left . serviceError InvalidParams . T.pack) Right (parseEither p v)

showT :: Show a => a -> Text
showT = T.pack . show
