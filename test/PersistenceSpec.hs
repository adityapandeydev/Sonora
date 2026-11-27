{-# LANGUAGE OverloadedStrings #-}
module PersistenceSpec (persistenceTests) where

import Control.Exception (SomeException, catch, displayException, try)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import System.Directory
import System.FilePath ((</>))

import Sonora.Generate.Score (GenSettings (..))
import Sonora.Project.Migration
import Sonora.Project.Persistence
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Synth.Patch (Patch (..), lookupPresetPatch)
import Sonora.Synth.Voice (VoiceConfig (..))

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

score3, score2 :: Text
score3 = T.unlines ["title \"Saved\"", "tempo 100", "track lead (lead):", "    C4 quarter", "track bass (bass):", "    C2 half", "track pad (pad):", "    chord(C4, E4, G4) whole"]
score2 = T.unlines ["title \"Saved\"", "tempo 100", "track lead (lead):", "    C4 quarter", "track bass (bass):", "    C2 half"]

idOf :: Text -> Project -> TrackId
idOf name p = case [ trackId t | t <- projectTracks p, trackScoreName t == name ] of
    (i : _) -> i
    [] -> error "no such track"

run :: [Project -> Command] -> Project -> Project
run cs p0 = foldl (\p c -> either (error . show) id (applyCommand (c p) p)) p0 cs

-- | A project using everything a file has to hold.
rich :: Project
rich = run
    [ \p -> SetTrackPatch (idOf "lead" p) (fromJust (lookupPresetPatch "chime"))
    , \p -> UpdateTrackMix (idOf "bass" p) noMixUpdate { updateGain = Just 0.4, updatePan = Just (-0.3), updateMute = Just True }
    , \p -> UpdateTrackMix (idOf "pad" p) noMixUpdate { updateSolo = Just True, updateGain = Just 1.7 }
    , const (SetMasterGain 0.8)
    , const (SetScoreText score2)                     -- pad retires with its settings
    , const (SetScoreText (score2 <> "track oops (lead\n"))  -- the editor text has an error now
    ] (newProject score3)

versions :: [SavedVersion]
versions =
    [ SavedVersion "Generated: C major, 120 bpm, 8 bars, seed 7" "track a (lead):\n    C4 whole\n" (Just (GenSettings 7 "C" "major" 120 8))
    , SavedVersion "Before generating" "track b (bass):\n    C2 whole\n" Nothing ]

atRevision0 :: Project -> Project
atRevision0 p = p { projectRevision = Revision 0 }

-- | Change a field of the encoded file.
tweak :: (Object -> Object) -> BL.ByteString -> BL.ByteString
tweak f bytes = case decode bytes of
    Just (Object o) -> encode (Object (f o))
    _ -> error "not an object"

at :: Text -> Value -> Object -> Object
at k v = KM.insert (Key.fromText k) v

overTracks :: (Value -> Value) -> Object -> Object
overTracks f o = case KM.lookup "tracks" o of
    Just (Array ts) -> KM.insert "tracks" (Array (Vec.imap (\i t -> if i == 0 then f t else t) ts)) o
    _ -> o

field :: Text -> Value -> Value -> Value
field k v (Object o) = Object (KM.insert (Key.fromText k) v o)
field _ _ x = x

persistenceTests :: IO Bool
persistenceTests = do
    putStrLn "\n--- Project files ---"
    tmp <- getTemporaryDirectory
    let dir = tmp </> "sonora-test-projects"
    removePathForcibly dir
    createDirectoryIfMissing True dir
    let bytes = encodeProject (ProjectFile rich versions)
        loads b = decodeProject b
        damaged b = case loads b of { Left (Damaged _) -> True; _ -> False }
    rs <- sequence
        [ check "the test project uses retired tracks, overrides, mixes and an invalid edit" $
            return (length (projectRetired rich) == 1 && not (scoreIsCurrent rich) && isJust (projectValidScore rich))
        , check "a saved project opens as the same project, versions included" $
            return (loads bytes == Right (ProjectFile (atRevision0 rich) versions))
        , check "track ids survive, and new tracks never reuse them" $ do
            p <- either (fail . show) (return . fileProject) (loads bytes)
            let p' = run [const (SetScoreText (score2 <> "track drums (pluck):\n    C3 quarter\n"))] p
                ids = map trackId (projectTracks p') ++ map retiredId (projectRetired p')
            return (map trackId (projectTracks p) == map trackId (projectTracks rich)
                    && length ids == length (Map.fromList [ (i, ()) | i <- ids ]))
        , check "files from a newer Sonora, unknown versions and other files are refused" $
            return (loads (tweak (at "version" (Number 2)) bytes) == Left (NewerFormat 2)
                    && loads (tweak (at "version" (Number 0)) bytes) == Left (UnknownFormat 0)
                    && loads (tweak (at "format" (String "something-else")) bytes) == Left NotAProject
                    && loads "{\"hello\": 1}" == Left NotAProject
                    && (case loads "not json at all" of { Left (NotJson _) -> True; _ -> False })
                    && (case loads (BL.take (BL.length bytes `div` 2) bytes) of { Left (NotJson _) -> True; _ -> False }))
        , check "damaged contents are refused with a reason" $ do
            let badPatch = (fromJust (lookupPresetPatch "lead")) { patchVoice = (patchVoice (fromJust (lookupPresetPatch "lead"))) { voiceFilterCutoff = 5 } }
            return (damaged (tweak (overTracks (field "patch" (toJSON badPatch))) bytes)
                    && damaged (tweak (overTracks (field "mix" (object ["gain" .= (3 :: Double), "pan" .= (0 :: Double), "mute" .= False, "solo" .= False]))) bytes)
                    && damaged (tweak (overTracks (field "name" (String "renamed"))) bytes)
                    && damaged (tweak (overTracks (field "id" (String "t2"))) bytes)
                    && damaged (tweak (at "master" (object ["gain" .= (-1 :: Double)])) bytes)
                    && damaged (tweak (KM.delete "score") bytes))
        , check "a saved valid score that no longer compiles keeps its tracks' settings as retired" $ do
            let broken = tweak (at "score" (object ["text" .= projectScoreText rich, "lastValid" .= ("track x (lead\n" :: Text)])) bytes
            return $ case loads broken of
                Right (ProjectFile p _) -> null (projectTracks p) && projectValidScore p == Nothing
                    && map retiredName (projectRetired p) == ["lead", "bass", "pad"]
                    && fmap retiredPatch (take 1 (projectRetired p)) == [lookupPresetPatch "chime"]
                _ -> False
        , check "migrations run in order and set the version; gaps and refusals are errors" $ do
            let steps = Map.fromList
                    [ (1, \v -> Right (field "a" (Number 1) v))
                    , (2, \v -> case v of { Object o | KM.member "a" o -> Right (field "b" (Number 2) v); _ -> Left "no a" }) ]
                start = object ["version" .= (1 :: Int)]
            return (migrateWith 3 steps 1 start == Right (object ["version" .= (3 :: Int), "a" .= (1 :: Int), "b" .= (2 :: Int)])
                    && migrateWith 3 steps 2 start == Left (MigrationFailed 2 "no a")
                    && migrateWith 4 steps 1 start == Left (NoMigration 3)
                    && migrateWith 3 steps 5 start == Left (NewerVersion 5)
                    && currentVersion == 1 && Map.null migrations)
        , check "a failed write leaves the existing file as it was, with nothing left over" $ do
            let path = dir </> "song.sonoraproj"
            writeAtomically path bytes
            createDirectoryIfMissing True (path ++ ".part")   -- the next write cannot create its temporary file
            r <- try (writeAtomically path "{}") :: IO (Either SomeException ())
            removeDirectory (path ++ ".part")
            after <- BL.readFile path
            files <- listDirectory dir
            loaded <- readProjectFile path
            return (either (const True) (const False) r && after == bytes && files == ["song.sonoraproj"]
                    && loaded == Right (ProjectFile (atRevision0 rich) versions))
        , check "reading a missing file says so" $ do
            missing <- readProjectFile (dir </> "nope.sonoraproj")
            return (case missing of { Left (Unreadable _) -> True; _ -> False })
        ]
    return (and rs)
