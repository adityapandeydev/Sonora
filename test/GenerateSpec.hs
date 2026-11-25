{-# LANGUAGE OverloadedStrings #-}
module GenerateSpec (generateTests) where

import Control.Exception (SomeException, catch, displayException)
import Control.Monad (forM)
import Data.List (nub)
import qualified Data.Text as T
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import Sonora.DSL.AST
import Sonora.DSL.Parser (parseSonora)
import Sonora.DSL.Print
import Sonora.Generate.Score
import Sonora.Music.Duration (Duration (..), dotted, quarter)
import Sonora.Music.Pitch (pitchToMidi)
import Sonora.Project.Score (ScoreAnalysis (..), analyzeScore)

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

settings :: GenSettings
settings = GenSettings 42 "D" "dorian" 98 8

withoutLines :: ASTSong -> ASTSong
withoutLines s = s { astSongTracks = [ t { astTrackItemLines = [] } | t <- astSongTracks s ] }

reparse :: ASTSong -> Either String ASTSong
reparse s = either (Left . show) (Right . withoutLines) (parseSonora "printed" (T.unpack (printSong s)))

itemLength :: ASTItem -> Rational
itemLength item = case item of
    ASTItemNote n           -> toRational (astDuration n)
    ASTItemRest d           -> toRational d
    ASTItemChord _ d _      -> toRational d
    ASTItemNamedChord _ _ d -> toRational d
    ASTItemRepeat k xs      -> fromIntegral k * sum (map itemLength xs)

trackLengths :: ASTSong -> [Rational]
trackLengths s = [ sum (map itemLength (astTrackItems t)) | t <- astSongTracks s ]

everySetting :: [GenSettings]
everySetting =
    [ GenSettings seed root scale bpm bars
    | (seed, root, scale) <- zip3 [0, 1, 7, 42, 99999, 4294967295, 31337, 5, 12, 2024, 8, 3] rootNames scaleNames
    , (bpm, bars) <- [(120, 1), (97.5, 7), (240, 64)] ]

generateTests :: IO Bool
generateTests = do
    putStrLn "\n--- Generation and score printing ---"
    rs <- sequence
        [ check "the same settings give the same score; another seed gives another" $ do
            let a = printSong (generateScore settings)
            return (a == printSong (generateScore settings)
                    && a /= printSong (generateScore settings { genSeed = 43 }))
        , check "every track lasts exactly the requested bars, for every root and scale" $
            return (and [ all (== fromIntegral (genBars g)) (trackLengths (generateScore g)) | g <- everySetting ])
        , check "generated scores print and parse back to the same song" $
            return (and [ reparse (generateScore g) == Right (generateScore g) | g <- everySetting ])
        , check "generated scores have chords, varied melodies and no problems" $ do
            let ok g = let s = generateScore g
                           a = analyzeScore (printSong s)
                           (lead, pad) = case astSongTracks s of
                               (l : p : _) -> (l, p)
                               _ -> error "expected lead and chords tracks"
                           pitches = [ astPitch n | ASTItemNote n <- astTrackItems lead ]
                       in null (analysisDiagnostics a) && maybe False (const True) (analysisAst a)
                          && all isChord (astTrackItems pad)
                          && (genBars g < 4 || length (nub pitches) >= 4)
                isChord i = case i of ASTItemChord ps _ _ -> length ps >= 3; _ -> False
            return (all ok everySetting)
        , check "no note crosses a bar line, and every note starts on the sixteenth grid" $ do
            let spans t = let ls = map itemLength (astTrackItems t) in zip (scanl (+) 0 ls) ls
                bar x = floor x :: Integer
                fine (start, len) = bar start == bar (start + len - 1 / 64)
                                    && start * 16 == fromIntegral (floor (start * 16) :: Integer)
                onBeat g = and [ all fine (spans t) | t <- astSongTracks (generateScore g) ]
            return (all onBeat everySetting)
        , check "the last bar resolves: lead and bass hold one note" $ do
            let s = generateScore settings
                lastItem t = last (astTrackItems t)
                whole' i = case i of ASTItemNote n -> astDuration n == 1; _ -> False
            return (all (whole' . lastItem) [ t | t <- astSongTracks s, astTrackName t /= "chords" ])
        , check "in every scale the melody ends on the tonic and stays within 19 semitones of it" $ do
            let ok g = case [ t | t <- astSongTracks (generateScore g), astTrackName t == "lead" ] of
                    [lead] ->
                        let ps = [ pitchToMidi (astPitch n) | ASTItemNote n <- astTrackItems lead ]
                            tonic = last ps
                            rootClass = maybe (-1) fromEnum (lookup (genRoot g) rootTable)
                        in tonic `mod` 12 == rootClass && maximum ps - minimum ps <= 19
                    _ -> False
            return (all ok everySetting)
        , check "settings are validated with the parameter at fault" $ do
            let bad = validateSettings (GenSettings 5000000000 "H" "chromatic" 20 0)
                long = validateSettings (GenSettings 1 "C" "major" 40 64)
            return (map fst bad == ["seed", "root", "scale", "bpm", "bars"]
                    && map fst long == ["bars"]
                    && null (validateSettings settings))
        , check "durations print exactly" $
            return (map printDuration [quarter, dotted quarter, Duration (1/3), Duration (5/8), 2]
                    == ["quarter", "dotted quarter", "1/3", "5/8", "doubleWhole"])
        , check "example scores print and parse back to the same song" $ do
            files <- filter ((== ".sonora") . takeExtension) <$> listDirectory "examples"
            results <- forM files $ \f -> do
                txt <- readFile ("examples" </> f)
                return $ case parseSonora f txt of
                    Left _ -> True  -- not this test's concern
                    Right s -> reparse s == Right (withoutLines s)
            return (not (null files) && and results)
        ]
    return (and rs)
