{-# LANGUAGE OverloadedStrings #-}
module LayoutSpec (layoutTests) where

import Control.Exception (SomeException, catch, displayException)
import Data.Aeson (Value (..), decode, encode)
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T

import Sonora.Project.Layout
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Service.Session

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

score :: Text
score = T.unlines
    [ "title \"T\""            -- 1
    , "tempo 120"              -- 2
    , "time 3/4"               -- 3
    , ""                       -- 4
    , "track a (lead):"        -- 5
    , "    # opening"          -- 6
    , "    C4 quarter"         -- 7
    , "    chord(C4, E4) half" -- 8
    , "    rest quarter"       -- 9
    , "    repeat 2:"          -- 10
    , "        D4 eighth"      -- 11
    , "        E4 eighth"      -- 12
    , ""                       -- 13
    , "track b (bass):"        -- 14
    , "    C2 whole"           -- 15
    ]

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

tests :: [(String, IO Bool)]
tests =
    [ ("Outline finds track blocks, also in text with errors", return $
        let ok = outline score
            broken = outline (T.replace "track b (bass):" "track b (bass" score <> "track c (pad):\n  oops\n")
        in ok == Outline 16 [OutlineTrack "a" 5 13, OutlineTrack "b" 14 16]
           && map outlineName (outlineTracks broken) == ["a", "b", "c"]
           && map outlineHeader (outlineTracks broken) == [5, 14, 16]
           && outlineLast (last (outlineTracks broken)) == outlineLines broken
           && outline "" == Outline 1 []
           && outline "  # track nope\n" == Outline 2 [])

    , ("Song info gives tempo, metre, and beats per bar", return $
        case projectValidScore (newProject score) of
            Just v -> songInfo (validAst v) == SongInfo "T" 120 (3, 4) 3
            Nothing -> False)

    , ("Each item line knows when it sounds; repeats count from their first line", return $
        case timing (newProject score) of
            Just [a, b] ->
                let ls = [ (lineNumber l, lineStart l, lineEnd l, lineBeat l) | l <- timingLines a ]
                    expect = [(7, 0, 0.5, 0), (8, 0.5, 1.5, 1), (9, 1.5, 2, 3), (10, 2, 3, 4)]
                    notesOf n = [ (s, e, p) | NoteEvent s e p l <- timingNotes a, l == n ]
                in length ls == 4
                   && and (zipWith (\(n, s, e, bt) (n', s', e', bt') -> n == n' && near s s' && near e e' && near bt bt') ls expect)
                   && sort (map (\(_, _, p) -> p) (notesOf 8)) == [60, 64]
                   && length (notesOf 10) == 4 && null (notesOf 9)
                   && map lineNumber (timingLines b) == [15]
                   && timingTrack a == TrackId "t1" && timingTrack b == TrackId "t2"
            _ -> False)

    , ("Timing version moves only when the playable score changes", do
        s <- newSession defaultSessionConfig { sessionInitialScore = score }
        let emit _ = return ()
            version v = case v of
                Object o -> KM.lookup "timingVersion" o
                _ -> Nothing
        v0 <- projectView s
        v1 <- setScore s emit (score <> "track c (lead\n")
        v2 <- setScore s emit (score <> "\n")
        v3 <- setScore s emit (score <> "\n")
        t <- scoreTiming s
        closeSession s
        return $ version v0 == Just (Number 0) && version v1 == Just (Number 0)
            && version v2 == Just (Number 2) && version v3 == Just (Number 2)
            && (decode (encode t) :: Maybe Value) /= Nothing)
    ]

layoutTests :: IO Bool
layoutTests = do
    putStrLn "--- Running Score Layout Tests ---"
    and <$> mapM (uncurry check) tests
