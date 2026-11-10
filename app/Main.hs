{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Options.Applicative hiding (many)
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension)
import System.IO (hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import Text.Parsec (parse, spaces, try, many, errorPos)
import Text.Parsec.Pos (sourceLine, sourceColumn)

import qualified Data.Vector.Storable as V
import Sonora
import Sonora.Audio.Backend
import Sonora.Audio.Device (withDeviceBackend)

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
    , poMaxSeconds :: !(Maybe Double)
    }

data GenerateOptions = GenerateOptions
    { goSeed      :: !Word64
    , goScale     :: !String
    , goBars      :: !Int
    , goBPM       :: !Double
    , goOutput    :: !(Maybe FilePath)
    , goRenderWav :: !(Maybe FilePath)
    , goJson      :: !Bool
    }

data InspectOptions = InspectOptions
    { ioInput :: !FilePath
    , ioJson  :: !Bool
    }

data SynthOptions = SynthOptions
    { soPreset    :: !String
    , soNotes     :: !(Maybe String)
    , soPitch     :: !String
    , soDuration  :: !Double
    , soCutoff    :: !(Maybe Double)
    , soResonance :: !(Maybe Double)
    , soAttack    :: !(Maybe Double)
    , soDecay     :: !(Maybe Double)
    , soSustain   :: !(Maybe Double)
    , soRelease   :: !(Maybe Double)
    , soOutput    :: !FilePath
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
    <*> option auto (long "sample-rate" <> value 44100 <> showDefault <> help "Sample rate in Hz")
    <*> optional (option auto (long "max-seconds" <> metavar "SECONDS" <> help "Stop playback after this many seconds")))

generateParser :: Parser Command
generateParser = CmdGenerate <$> (GenerateOptions
    <$> option auto (short 's' <> long "seed" <> value 42 <> showDefault <> help "Random seed for deterministic generation")
    <*> strOption (long "scale" <> value "C-Major" <> showDefault <> help "Scale format: <Root>-<Type> e.g. C-Major, A-Minor, F-Dorian")
    <*> option auto (short 'b' <> long "bars" <> value 8 <> showDefault <> help "Number of bars to generate")
    <*> option auto (long "bpm" <> value 120.0 <> showDefault <> help "Tempo in BPM")
    <*> optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT" <> help "Output .sonora score file path"))
    <*> optional (strOption (long "render" <> metavar "WAV" <> help "Directly render generated score to WAV file"))
    <*> switch (long "json" <> help "Output generation results in structured JSON format"))

inspectParser :: Parser Command
inspectParser = CmdInspect <$> (InspectOptions
    <$> strArgument (metavar "INPUT" <> help "Path to .sonora score file to inspect")
    <*> switch (long "json" <> help "Output inspection result in structured JSON format"))

synthParser :: Parser Command
synthParser = CmdSynth <$> (SynthOptions
    <$> strOption (short 'p' <> long "preset" <> value "piano" <> showDefault <> help "Preset: piano, lead, bass, pad, pluck, chime")
    <*> optional (strOption (short 'n' <> long "notes" <> metavar "NOTES" <> help "Notes sequence string"))
    <*> strOption (long "pitch" <> value "C4" <> showDefault <> help "Pitch for single-note audition (e.g. C4, A3, F#5)")
    <*> option auto (long "duration" <> value 1.0 <> showDefault <> help "Note duration in seconds")
    <*> optional (option auto (long "cutoff" <> help "Override filter cutoff in Hz"))
    <*> optional (option auto (long "resonance" <> help "Override filter resonance Q"))
    <*> optional (option auto (long "attack" <> help "Override amp envelope attack in seconds"))
    <*> optional (option auto (long "decay" <> help "Override amp envelope decay in seconds"))
    <*> optional (option auto (long "sustain" <> help "Override amp envelope sustain level (0.0 - 1.0)"))
    <*> optional (option auto (long "release" <> help "Override amp envelope release in seconds"))
    <*> strOption (short 'o' <> long "output" <> value "output.wav" <> showDefault <> help "Output WAV file"))

cliParser :: Parser Command
cliParser = subparser
    (  command "render"   (info renderParser   (progDesc "Render a .sonora file to WAV audio"))
    <> command "play"     (info playParser     (progDesc "Render a .sonora file and play it on the default audio device"))
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
    -- Console code pages on Windows are often not UTF-8; force it so help
    -- text and JSON output never fail to encode when stdout is a pipe.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    cmd <- execParser cliInfo
    case cmd of
        CmdRender ro   -> runRender ro
        CmdPlay po     -> runPlay po
        CmdGenerate go -> runGenerate go
        CmdInspect io  -> runInspect io
        CmdSynth so    -> runSynth so

-- Helper to escape strings for JSON
escapeJson :: String -> String
escapeJson = concatMap $ \ch -> case ch of
    '"'  -> "\\\""
    '\\' -> "\\\\"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    other -> [other]

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
    putStrLn $ "[Sonora] Parsing score: " ++ poInput
    parsed <- parseSonoraFile poInput
    astSong <- either (\err -> hPutStrLn stderr ("Syntax error: " ++ show err) >> exitFailure) return parsed
    let song = evaluateSong astSong
        cfg = defaultConfig { configSampleRate = poSampleRate }
        frames = runSignal (normalizeSignal 0.95 (renderSong song)) cfg
        total = V.length frames
        limit = maybe total (\s -> min total (max 0 (round (s * fromIntegral poSampleRate)))) poMaxSeconds
        seconds :: Int -> Double
        seconds n = fromIntegral n / fromIntegral poSampleRate
    putStrLn $ "[Sonora] Rendering '" ++ songTitle song ++ "'..."
    _ <- evaluate (V.length frames)
    result <- withDeviceBackend poSampleRate $ \backend -> do
        putStrLn $ "[Sonora] Output: " ++ backendDescription backend
        backendLoad backend SongVoice frames
        backendSetPlaying backend SongVoice True
        let loop = do
                threadDelay 50000
                st <- backendStatus backend SongVoice
                lost <- backendDeviceLost backend
                hPutStr stdout $ "\r[Sonora] " ++ showFixed (seconds (fromIntegral (statusPosition st)))
                              ++ " / " ++ showFixed (seconds total) ++ " s"
                hFlush stdout
                if lost
                    then return (Just "audio device was lost during playback")
                    else if statusPlaying st && fromIntegral (statusPosition st) < limit
                        then loop
                        else Nothing <$ backendSetPlaying backend SongVoice False
        loop
    putStrLn ""
    case result of
        Left err -> do
            hPutStrLn stderr $ "[Sonora] Audio output unavailable: " ++ audioErrorMessage err
            exitFailure
        Right (Just problem) -> do
            hPutStrLn stderr $ "[Sonora] Playback stopped: " ++ problem
            exitFailure
        Right Nothing -> putStrLn "[Sonora] Playback finished."
  where
    showFixed x = let t = fromIntegral (round (x * 10) :: Int) / 10 :: Double in show t

runGenerate :: GenerateOptions -> IO ()
runGenerate GenerateOptions{..} = do
    let (root, sType) = parseScaleString goScale
        song = generateProceduralSong (Seed goSeed) sType root (BPM goBPM) goBars
        dslContent = songToDSL song

    case goOutput of
        Just dslPath -> writeFile dslPath dslContent
        Nothing      -> return ()

    case goRenderWav of
        Just wavPath -> renderSongToWAV wavPath song
        Nothing      -> return ()

    if goJson
        then do
            putStrLn $ "{"
                ++ "\"status\":\"ok\","
                ++ "\"seed\":" ++ show goSeed ++ ","
                ++ "\"scale\":\"" ++ escapeJson goScale ++ "\","
                ++ "\"bars\":" ++ show goBars ++ ","
                ++ "\"bpm\":" ++ show goBPM ++ ","
                ++ "\"title\":\"" ++ escapeJson (songTitle song) ++ "\","
                ++ "\"dsl\":\"" ++ escapeJson dslContent ++ "\""
                ++ "}"
        else do
            putStrLn $ "[Sonora] Generating procedural music with seed " ++ show goSeed ++ "..."
            case goOutput of
                Just dslPath -> putStrLn $ "[Sonora] Saved procedural score to: " ++ dslPath
                Nothing      -> return ()
            case goRenderWav of
                Just wavPath -> putStrLn $ "[Sonora] Render complete: " ++ wavPath
                Nothing ->
                    when (goOutput == Nothing) $ do
                        let defPath = "procedural_seed_" ++ show goSeed ++ ".sonora"
                        writeFile defPath dslContent
                        putStrLn $ "[Sonora] Wrote score to default path: " ++ defPath

runInspect :: InspectOptions -> IO ()
runInspect InspectOptions{..} = do
    parsed <- parseSonoraFile ioInput
    case parsed of
        Left err -> do
            if ioJson
                then do
                    let pos = errorPos err
                        lineNo = sourceLine pos
                        colNo  = sourceColumn pos
                        msg    = show err
                    putStrLn $ "{"
                        ++ "\"status\":\"error\","
                        ++ "\"line\":" ++ show lineNo ++ ","
                        ++ "\"column\":" ++ show colNo ++ ","
                        ++ "\"message\":\"" ++ escapeJson msg ++ "\""
                        ++ "}"
                    exitFailure
                else do
                    hPutStrLn stderr $ "Syntax error: " ++ show err
                    exitFailure
        Right astSong -> do
            let song = evaluateSong astSong
            if ioJson
                then do
                    let tList = intercalate "," (map trackJson (songTracks song))
                    putStrLn $ "{"
                        ++ "\"status\":\"ok\","
                        ++ "\"title\":\"" ++ escapeJson (songTitle song) ++ "\","
                        ++ "\"bpm\":" ++ show (getBPM (songBPM song)) ++ ","
                        ++ "\"timeSignature\":\"" ++ show (tsNumerator (songTimeSig song))
                        ++ "/" ++ show (tsDenominator (songTimeSig song)) ++ "\","
                        ++ "\"trackCount\":" ++ show (length (songTracks song)) ++ ","
                        ++ "\"tracks\":[" ++ tList ++ "]"
                        ++ "}"
                else do
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
    trackJson Track{..} =
        let events = linearize noteDuration trackMusic
            dur = musicDuration noteDuration trackMusic
            sec = durationToSeconds (BPM 120) dur
        in "{"
            ++ "\"name\":\"" ++ escapeJson trackName ++ "\","
            ++ "\"instrument\":\"" ++ escapeJson (instName trackInstrument) ++ "\","
            ++ "\"notesCount\":" ++ show (length events) ++ ","
            ++ "\"durationSeconds\":" ++ show sec
            ++ "}"

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
    let baseInst = fromMaybe defaultInstrument (lookupInstrument soPreset)
        baseVoice = instVoice baseInst
        v1 = maybe baseVoice (\cut -> baseVoice { voiceFilterCutoff = cut }) soCutoff
        v2 = maybe v1 (\r -> v1 { voiceFilterRes = r }) soResonance
        origAmp = voiceAmpEnv v2
        ampA = fromMaybe (adsrAttack origAmp) soAttack
        ampD = fromMaybe (adsrDecay origAmp) soDecay
        ampS = fromMaybe (adsrSustain origAmp) soSustain
        ampR = fromMaybe (adsrRelease origAmp) soRelease
        v3 = v2 { voiceAmpEnv = adsr ampA ampD ampS ampR }
        inst = baseInst { instVoice = v3 }

    case soNotes of
        Just notesStr | not (null notesStr) -> do
            let parsedNotes = parse (many (try (noteItem <* spaces))) "synth-cli" notesStr
            case parsedNotes of
                Left err -> do
                    hPutStrLn stderr $ "Error parsing note string: " ++ show err
                    exitFailure
                Right notes -> do
                    let music = line notes
                    renderMusicToWAV soOutput (BPM 120) inst music
        _ -> do
            let parsedPitch = parse (pitchClassParser <* spaces) "pitch-cli" soPitch
            case parsedPitch of
                Left err -> do
                    hPutStrLn stderr $ "Error parsing pitch '" ++ soPitch ++ "': " ++ show err
                    exitFailure
                Right pc -> do
                    let dur = Duration (toRational soDuration / 2)
                        music = Prim (note pc dur)
                    renderMusicToWAV soOutput (BPM 120) inst music
  where
    noteItem = do
        pc <- pitchClassParser
        _ <- spaces
        durVal <- durationParser
        return (Prim (note pc durVal))

-- Helper to parse scale string "C-Major", "A-Minor", etc.
parseScaleString :: String -> (PitchClass, ScaleType)
parseScaleString rawStr =
    case break (== '-') rawStr of
        (rStr, '-' : tStr) ->
            let root = case parse (pitchClassParser <* spaces) "root" (rStr ++ "4") of
                    Right pVal -> pVal
                    Left _  -> c4
                sType = case tStr of
                    "Major"           -> Major
                    "NaturalMinor"    -> NaturalMinor
                    "Minor"           -> NaturalMinor
                    "HarmonicMinor"   -> HarmonicMinor
                    "Dorian"          -> Dorian
                    "Phrygian"        -> Phrygian
                    "Lydian"          -> Lydian
                    "Mixolydian"      -> Mixolydian
                    "Aeolian"         -> NaturalMinor
                    "Locrian"         -> Locrian
                    "Pentatonic"      -> MajorPentatonic
                    "PentatonicMajor" -> MajorPentatonic
                    "PentatonicMinor" -> MinorPentatonic
                    "MajorPentatonic" -> MajorPentatonic
                    "MinorPentatonic" -> MinorPentatonic
                    "Blues"           -> Blues
                    _                 -> Major
            in (root, sType)
        _ -> (c4, Major)
