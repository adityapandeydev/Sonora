{-# LANGUAGE OverloadedStrings #-}
-- | Bringing project files from older format versions to the current one.
--
-- Each migration takes a file's JSON at version @n@ to version @n + 1@ and
-- sets its @"version"@ field. Files newer than this build are refused rather
-- than guessed at. Version 1 is the first format, so no migrations exist yet;
-- a format change adds one entry to 'migrations' and bumps 'currentVersion'.
module Sonora.Project.Migration
    ( currentVersion
    , Migration
    , migrations
    , MigrationError (..)
    , migrate
    , migrateWith
    ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)

currentVersion :: Int
currentVersion = 1

-- | From one version to the next.
type Migration = Value -> Either Text Value

-- | Keyed by the version they upgrade from.
migrations :: Map.Map Int Migration
migrations = Map.empty

data MigrationError
    = NewerVersion !Int            -- ^ Saved by a newer build.
    | NoMigration !Int             -- ^ Too old, or not a known version.
    | MigrationFailed !Int !Text   -- ^ A migration from this version refused the file.
    deriving (Eq, Show)

migrate :: Int -> Value -> Either MigrationError Value
migrate = migrateWith currentVersion migrations

-- | Upgrade a file at a version, step by step, to the target version.
migrateWith :: Int -> Map.Map Int Migration -> Int -> Value -> Either MigrationError Value
migrateWith target steps from v
    | from == target = Right v
    | from > target = Left (NewerVersion from)
    | otherwise = case Map.lookup from steps of
        Nothing -> Left (NoMigration from)
        Just step -> case step v of
            Left why -> Left (MigrationFailed from why)
            Right v' -> migrateWith target steps (from + 1) (setVersion (from + 1) v')
  where
    setVersion n (Object o) = Object (KM.insert "version" (toJSON n) o)
    setVersion _ other = other
