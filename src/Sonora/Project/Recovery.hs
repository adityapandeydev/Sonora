{-# LANGUAGE OverloadedStrings #-}
-- | Work kept safe between saves.
--
-- While a project has unsaved changes, the session writes it to one
-- autosave file in the recovery folder. The autosave is an ordinary project
-- (score as typed, last valid score, sounds, mix, versions) wrapped with
-- where it came from and when:
--
-- > { "recovery": { "file": path | null, "savedAt": "2026-11-28T10:15:02Z" },
-- >   "project": { ...project file... } }
--
-- When the work is saved or deliberately discarded the autosave is removed,
-- so an autosave found at start-up means Sonora stopped with unsaved work.
-- One that cannot be read is moved aside, never deleted.
module Sonora.Project.Recovery
    ( RecoveryInfo (..)
    , Found (..)
    , autosaveFile
    , encodeAutosave
    , decodeAutosave
    , writeAutosave
    , findAutosave
    , removeAutosave
    , setAsideAutosave
    ) where

import Control.Exception (IOException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath ((</>))

import Sonora.Project.Persistence

data RecoveryInfo = RecoveryInfo
    { recoveryFile  :: !(Maybe FilePath)  -- ^ The project file the work belongs to.
    , recoverySaved :: !Text              -- ^ When the autosave was written (UTC, ISO 8601).
    } deriving (Eq, Show)

data Found
    = NoAutosave
    | Recoverable !RecoveryInfo !ProjectFile
    | Unrecoverable !LoadError
    deriving (Eq, Show)

autosaveFile :: FilePath -> FilePath
autosaveFile dir = dir </> "autosave.sonoraproj"

encodeAutosave :: RecoveryInfo -> ProjectFile -> BL.ByteString
encodeAutosave (RecoveryInfo file saved) pf = encode $ object
    [ "recovery" .= object ["file" .= file, "savedAt" .= saved]
    , "project"  .= projectValue pf ]

decodeAutosave :: BL.ByteString -> Either LoadError (RecoveryInfo, ProjectFile)
decodeAutosave bytes = do
    v <- either (Left . NotJson . T.pack) Right (eitherDecode bytes)
    (info, project) <- either (const (Left NotAProject)) Right (parseEither wrapper v)
    pf <- decodeProjectValue project
    return (info, pf)
  where
    wrapper = withObject "autosave" $ \o -> do
        r <- o .: "recovery"
        info <- withObject "recovery" (\x -> RecoveryInfo <$> x .:? "file" <*> x .: "savedAt") r
        (,) info <$> o .: "project"

-- | Write the autosave, stamped with the current time.
writeAutosave :: FilePath -> Maybe FilePath -> ProjectFile -> IO ()
writeAutosave dir file pf = do
    now <- getCurrentTime
    createDirectoryIfMissing True dir
    writeAtomically (autosaveFile dir) (encodeAutosave (RecoveryInfo file (T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))) pf)

findAutosave :: FilePath -> IO Found
findAutosave dir = do
    let path = autosaveFile dir
    exists <- doesFileExist path
    if not exists then return NoAutosave else do
        bytes <- try (BL.readFile path >>= \b -> BL.length b `seq` return b)
        return $ case bytes of
            Left e -> Unrecoverable (Unreadable (T.pack (show (e :: IOException))))
            Right b -> either Unrecoverable (uncurry Recoverable) (decodeAutosave b)

removeAutosave :: FilePath -> IO ()
removeAutosave dir = do
    r <- try (removeFile (autosaveFile dir)) :: IO (Either IOException ())
    return (either (const ()) id r)

-- | Keep an unreadable autosave out of the way, under a dated name.
setAsideAutosave :: FilePath -> IO (Maybe FilePath)
setAsideAutosave dir = do
    now <- getCurrentTime
    let target = dir </> ("damaged-autosave-" ++ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now ++ ".sonoraproj")
    r <- try (renameFile (autosaveFile dir) target) :: IO (Either IOException ())
    return (either (const Nothing) (const (Just target)) r)
