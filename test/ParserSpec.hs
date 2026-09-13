module ParserSpec (parserTests) where

import Sonora.DSL.Evaluator
import Sonora.DSL.Parser
import Sonora.Music.Duration
import Sonora.Render.Engine

testScoreText :: String
testScoreText = unlines
    [ "# Test Score"
    , "title \"Unit Test Suite\""
    , "tempo 130"
    , "time 3/4"
    , ""
    , "instrument leadSynth:"
    , "    oscillator saw"
    , "    cutoff 1800"
    , "    resonance 1.2"
    , "    envelope 0.01 0.1 0.6 0.2"
    , ""
    , "track melody (leadSynth):"
    , "    C4 quarter mf"
    , "    E4 quarter"
    , "    G4 half f"
    , "    rest quarter"
    , "    chord(C4, E4, G4) whole"
    , ""
    , "track bass (bass):"
    , "    C2 half"
    , "    G2 half"
    ]

testParserRoundtrip :: Bool
testParserRoundtrip =
    case parseSonora "test" testScoreText of
        Left _ -> False
        Right astSong ->
            let song = evaluateSong astSong
            in songTitle song == "Unit Test Suite"
               && getBPM (songBPM song) == 130.0
               && tsNumerator (songTimeSig song) == 3
               && tsDenominator (songTimeSig song) == 4
               && length (songTracks song) == 2

parserTests :: IO Bool
parserTests = do
    putStrLn "--- Running Sonora DSL Parser Tests ---"
    putStrLn $ "  DSL Score Parsing & Evaluation: " ++ if testParserRoundtrip then "PASSED" else "FAILED"
    return testParserRoundtrip
