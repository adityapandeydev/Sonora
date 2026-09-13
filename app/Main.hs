{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>), replaceExtension, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand)
import Text.Parsec (parse, spaces, string, try)

import Sonora

-- | CLI Subcommands
data Command
    = CmdRender RenderOptions
    | CmdPlay PlayOptions
    | CmdGenerate GenerateOptions
    | CmdInspect InspectOptions
    | CmdSynth SynthOptions

data RenderOptions = RenderOptions
    { roInput      :: !FilePath
    , roOutput     :: !(Maybe FilePath)
    , roSampleRate :: !Int
    , roBitDepth   :: !String
    }

data PlayOptions = PlayOptions
    { poInput      :: !FilePath
    , poSampleRate :: !Int
    }

data GenerateOptions = GenerateOptions
    { goSeed      :: !Word64
    , goScale     :: !String
    , goBars      :: !Int
    , goBPM       :: !Double
    , goOutput    :: !(Maybe FilePath)
    , goRenderWav :: !(Maybe FilePath)
    }

data InspectOptions = InspectOptions
    { ioInput :: !FilePath
    }

data SynthOptions = SynthOptions
    { soPreset :: !String
    , soNotes  :: !String
    , soOutput :: !FilePath
    }

-- CLI Parser
renderParser :: Parser Command
renderParser = CmdRender <$> (RenderOptions
    <$> strArgument (metavar "INPUT" <> help "Path to .sonora score file")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT" <> help "Output WAV path (default: <input>.wav)"))
    <*> option auto (long "sample-rate" <> value 44100 <> showDefault <> help "Sample rate in Hz")
    <*> strOption (long "bit-depth" <> value "16" <> showDefault <> help "Bit depth: 16, 24, or 32float"))

playParser :: Parser Command
playParser = CmdPlay <$> (PlayOptions
    <$> strArgument (metavar "INPUT" <> help "Path to .sonora score file to render and play")
    <*> option auto (long "sample-rate" <> value 44100 <> showDefault <> help "Sample rate in Hz"))

generateParser :: Parser Command
generateParser = CmdGenerate <$> (GenerateOptions
    <$> option auto (short 's' <> long "seed" <> value 42 <> showDefault <> help "Random seed for deterministic generation")
    <*> strOption (long "scale" <> value "C-Major" <> showDefault <> help "Scale format: <Root>-<Type> e.g. C-Major, A-Minor, F-Dorian")
    <*> option auto (short 'b' <> long "bars" <> value 8 <> showDefault <> help "Number of bars to generate")
    <*> option auto (long "bpm" <> value 120.0 <> showDefault <> help "Tempo in BPM")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT" <> help "Output .sonora score file path"))
    <*> optional (strOption (long "render" <> metavar "WAV" <> help "Directly render generated score to WAV file")))

inspectParser :: Parser Command
inspectParser = CmdInspect <$> (InspectOptions
    <$> strArgument (metavar "INPUT" <> help "Path to .sonora score file to inspect"))

synthParser :: Parser Command
synthParser = CmdSynth <$> (SynthOptions
    <$> strOption (short 'p' <> long "preset" <> value "piano" <> showDefault <> help "Preset: piano, lead, bass, pad, pluck, chime")
    <*> strOption (short 'n' <> long "notes" <> metavar "NOTES" <> help "Notes string e.g. 'C4 quarter E4 quarter G4 half'")
    <*> strOption (short 'o' <> long "output" <> value "output.wav" <> showDefault <> help "Output WAV file"))

cliParser :: Parser Command
cliParser = subparser
    (  command "render"   (info renderParser   (progDesc "Render a .sonora file to WAV audio"))
    <> command "play"     (info playParser     (progDesc "Render and immediately play a .sonora file"))
    <> command "generate" (info generateParser (progDesc "Procedurally generate a deterministic score and/or WAV"))
    <> command "inspect"  (info inspectParser  (progDesc "Inspect score structure, tracks, duration, and metadata"))
    <> command "synth"    (info synthParser    (progDesc "Quickly synthesize a sequence of notes to WAV"))
    )

cliInfo :: ParserInfo Command
cliInfo = info (cliParser <**> helper)
    ( fullDesc
   <> header "Sonora — Functional Music Synthesis & Composition Engine"
   <> progDesc "Compose, synthesize, and render procedural and declarative music in Haskell."
    )

main :: IO ()
main = do
    cmd <- execParser cliInfo
    case cmd of
        CmdRender ro   -> runRender ro
        CmdPlay po     -> runPlay po
        CmdGenerate go -> runGenerate go
        CmdInspect io  -> runInspect io
        CmdSynth so    -> runSynth so

-- Command Implementations

runRender :: RenderOptions -> IO ()
runRender RenderOptions{..} = do
    putStrLn $ "[Sonora] Parsing score: " ++ roInput
    parsed <- parseSonoraFile roInput
    case parsed of
        Left err -> do
            hPutStrLn stderr $ "Syntax error: " ++ show err
            exitFailure
        Right astSong -> do
            let song = evaluateSong astSong
                outPath = fromMaybe (replaceExtension roInput "wav") roOutput
                cfg = defaultConfig { configSampleRate = roSampleRate }
                depth = case roBitDepth of
                    "24"      -> BitDepth24
                    "32float" -> BitDepth32Float
                    _         -> BitDepth16
            putStrLn $ "[Sonora] Rendering '" ++ songTitle song ++ "' at "
                     ++ show roSampleRate ++ " Hz (" ++ roBitDepth ++ "-bit)..."
            let sig = normalizeSignal 0.95 (renderSong song)
            writeWAVFile outPath depth cfg sig
            putStrLn $ "[Sonora] Successfully rendered to: " ++ outPath

runPlay :: PlayOptions -> IO ()
runPlay PlayOptions{..} = do
    tempDir <- getTemporaryDirectory
    let tempWav = tempDir </> (takeBaseName poInput ++ "_preview.wav")
    runRender (RenderOptions poInput (Just tempWav) poSampleRate "16")
    putStrLn $ "[Sonora] Playing preview via system audio player..."
    -- On Windows, invoke PowerShell Media.SoundPlayer
    let playCmd = "powershell -c \"(New-Object Media.SoundPlayer '" ++ tempWav ++ "').PlaySync()\""
    callCommand playCmd

runGenerate :: GenerateOptions -> IO ()
runGenerate GenerateOptions{..} = do
    putStrLn $ "[Sonora] Generating procedural music with seed " ++ show goSeed ++ "..."
    let (root, sType) = parseScaleString goScale
        song = generateProceduralSong (Seed goSeed) sType root (BPM goBPM) goBars

    case goOutput of
        Just dslPath -> do
            writeFile dslPath (songToDSL song)
            putStrLn $ "[Sonora] Saved procedural score to: " ++ dslPath
        Nothing -> return ()

    case goRenderWav of
        Just wavPath -> do
            putStrLn $ "[Sonora] Rendering procedural audio to: " ++ wavPath
            renderSongToWAV wavPath song
            putStrLn $ "[Sonora] Render complete: " ++ wavPath
        Nothing ->
            when (goOutput == Nothing) $ do
                let defPath = "procedural_seed_" ++ show goSeed ++ ".sonora"
                writeFile defPath (songToDSL song)
                putStrLn $ "[Sonora] Wrote score to default path: " ++ defPath

runInspect :: InspectOptions -> IO ()
runInspect InspectOptions{..} = do
    parsed <- parseSonoraFile ioInput
    case parsed of
        Left err -> do
            hPutStrLn stderr $ "Syntax error: " ++ show err
            exitFailure
        Right astSong -> do
            let song = evaluateSong astSong
            putStrLn "=================================================="
            putStrLn $ "  Title:          " ++ songTitle song
            putStrLn $ "  Tempo:          " ++ show (getBPM (songBPM song)) ++ " BPM"
            putStrLn $ "  Time Signature: " ++ show (tsNumerator (songTimeSig song))
                     ++ "/" ++ show (tsDenominator (songTimeSig song))
            putStrLn $ "  Track Count:    " ++ show (length (songTracks song))
            putStrLn "--------------------------------------------------"
            mapM_ inspectTrack (songTracks song)
            putStrLn "=================================================="
  where
    inspectTrack Track{..} = do
        let events = linearize noteDuration trackMusic
            dur = musicDuration noteDuration trackMusic
            sec = durationToSeconds (BPM 120) dur
        putStrLn $ "  * Track: " ++ trackName
        putStrLn $ "    Instrument: " ++ instName trackInstrument
        putStrLn $ "    Notes:      " ++ show (length events)
        putStrLn $ "    Duration:   " ++ show dur ++ " (~" ++ take 5 (show sec) ++ " sec at 120 BPM)"

runSynth :: SynthOptions -> IO ()
runSynth SynthOptions{..} = do
    let inst = fromMaybe defaultInstrument (lookupInstrument soPreset)
    -- Parse simple notes string
    let parsedNotes = parse (many (try (noteItem <* spaces))) "synth-cli" soNotes
    case parsedNotes of
        Left err -> do
            hPutStrLn stderr $ "Error parsing note string: " ++ show err
            exitFailure
        Right notes -> do
            let music = line notes
            putStrLn $ "[Sonora] Synthesizing " ++ show (length notes) ++ " notes with " ++ soPreset ++ "..."
            renderMusicToWAV soOutput (BPM 120) inst music
            putStrLn $ "[Sonora] Exported to: " ++ soOutput
  where
    noteItem = do
        pc <- Sonora.c4 <$ string "C4" <|> Sonora.e4 <$ string "E4" <|> Sonora.g4 <$ string "G4"
           <|> Sonora.a4 <$ string "A4" <|> Sonora.d4 <$ string "D4" <|> Sonora.b4 <$ string "B4"
           <|> Sonora.f4 <$ string "F4"
        _ <- spaces
        d <- Sonora.quarter <$ string "quarter" <|> Sonora.half <$ string "half" <|> Sonora.whole <$ string "whole"
        return (Prim (note pc d))

-- Helper to parse scale string "C-Major", "A-Minor", etc.
parseScaleString :: String -> (PitchClass, ScaleType)
parseScaleString str =
    case break (== '-') str of
        ("C", "-Major") -> (c4, Major)
        ("A", "-Minor") -> (a4, NaturalMinor)
        ("F", "-Major") -> (f4, Major)
        ("G", "-Major") -> (g4, Major)
        ("D", "-Dorian") -> (d4, Dorian)
        ("E", "-Minor") -> (e4, NaturalMinor)
        _               -> (c4, Major)
