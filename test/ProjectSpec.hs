{-# LANGUAGE OverloadedStrings #-}
module ProjectSpec (projectTests) where

import Control.Monad (forM)
import Data.Aeson (decode, encode, eitherDecode)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.QuickCheck

import Sonora.DSL.AST (ASTSong (..), ASTTrack (..))
import Sonora.DSL.Evaluator (evaluateSong)
import Sonora.DSL.Parser (parseSonora)
import Sonora.Music.Composition (Music (..), line)
import Sonora.Music.Duration (BPM (..), eighth, half, quarter)
import Sonora.Music.Note (Articulation (..), Note (..), mf, mp)
import qualified Sonora.Music.Note as Dyn
import Sonora.Music.Pitch (c4, e4, g3)
import Sonora.Render.Engine (renderSong)
import Sonora.Signal.Core (AudioConfig (..), Signal (..))
import Sonora.Signal.Envelope (ADSR (..), EnvelopeCurve (..))
import Sonora.Signal.Filter (FilterType (..))
import Sonora.Signal.Oscillator (Waveform (..))
import Sonora.Synth.Instrument (defaultInstruments)
import Sonora.Synth.Synthesizer (renderComposition)
import Sonora.Synth.Voice (OscSetting (..), VoiceConfig (..), defaultOscSetting)
import Sonora.Project.Compile
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Synth.Patch

-------------------------------------------------------------------------------
-- Helpers

testCfg :: AudioConfig
testCfg = AudioConfig 8000 2

phrase :: Music Note
phrase = line [ Prim (Note c4 quarter mf Normal)
              , Prim (Note e4 eighth Dyn.f Staccato)
              , Rest eighth
              , Prim (Note g3 half mp Legato) ]

check :: String -> Bool -> IO Bool
check name passed = do
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

ok :: Either CommandError Project -> Project
ok = either (error . show) id

run :: [Command] -> Project -> Project
run cmds p = ok (applyCommands cmds p)

trackNamed :: Text -> Project -> TrackState
trackNamed n proj = fromJust (find ((== n) . trackScoreName) (projectTracks proj))

firstDiag :: Project -> Diagnostic
firstDiag proj = case projectDiagnostics proj of
    (x : _) -> x
    []      -> error "expected a diagnostic"

firstTrack :: CompiledProject -> CompiledTrack
firstTrack c = case compiledTracks c of
    (x : _) -> x
    []      -> error "expected a track"

score :: [Text] -> Text
score = T.unlines

baseScore :: Text
baseScore = score
    [ "title \"Test\"", "tempo 100", ""
    , "track lead (lead):", "    C4 quarter", "    E4 quarter", ""
    , "track bass (bass):", "    C2 half" ]

somePatch :: Patch
somePatch = fromJust (lookupPresetPatch "pad")

-------------------------------------------------------------------------------
-- Patches

-- | Every preset patch renders exactly like the built-in instrument it describes.
presetsBitExact :: Bool
presetsBitExact = and
    [ runSignal (renderComposition (BPM 120) inst phrase) testCfg
        == runSignal (renderComposition (BPM 120) (compilePatch patch) phrase) testCfg
    | (name, inst) <- Map.toList defaultInstruments
    , let patch = presetPatches Map.! T.pack name ]

genEffect :: Gen EffectSpec
genEffect = oneof
    [ DelayEffect <$> choose (0.001, 4) <*> choose (0, 0.95) <*> choose (0, 1)
    , ReverbEffect <$> choose (0, 1) <*> choose (0, 1)
    , ChorusEffect <$> choose (0.01, 10) <*> choose (0, 1) <*> choose (0, 1)
    , DistortionEffect <$> choose (1, 50)
    , TremoloEffect <$> choose (0.01, 20) <*> choose (0, 1) ]

genEnv :: Gen ADSR
genEnv = ADSR <$> choose (0, 20) <*> choose (0, 20) <*> choose (0, 1) <*> choose (0, 20)
              <*> elements [LinearCurve, ExponentialCurve]

genOsc :: Gen OscSetting
genOsc = OscSetting
    <$> oneof [ elements [Sine, Triangle, Sawtooth, WhiteNoiseGen, PinkNoiseGen], Square <$> choose (0.01, 0.99) ]
    <*> choose (0, 1) <*> choose (-48, 48) <*> choose (-100, 100) <*> choose (0, 2 * pi)

genPatch :: Gen Patch
genPatch = do
    name <- elements ["a", "lead 2", "Pad — wide", "x"]
    oscs <- choose (1, maxOscillators) >>= flip vectorOf genOsc
    voice <- VoiceConfig oscs
        <$> elements [LowPass, HighPass, BandPass, Notch]
        <*> choose (20, 20000) <*> choose (0.1, 20) <*> choose (-20000, 20000)
        <*> genEnv <*> genEnv
    effects <- choose (0, maxEffects) >>= flip vectorOf genEffect
    return (Patch name voice effects)

-- | Valid patches survive a JSON round trip unchanged and stay valid.
prop_patchJsonRoundTrip :: Property
prop_patchJsonRoundTrip = forAll genPatch $ \p ->
    validatePatch p === [] .&&. decode (encode p) === Just p

validationScenarios :: [(String, Bool)]
validationScenarios =
    let v = patchVoice somePatch
        badCutoff = somePatch { patchVoice = v { voiceFilterCutoff = 50000 } }
        noOscs = somePatch { patchVoice = v { voiceOscillators = [] } }
        badOsc = somePatch { patchVoice = v { voiceOscillators = [defaultOscSetting { oscSetMix = 1.5 }] } }
        badEffect = somePatch { patchEffects = [DelayEffect 0.2 0.99 0.5] }
        nanSustain = somePatch { patchVoice = v { voiceAmpEnv = (voiceAmpEnv v) { adsrSustain = 0 / 0 } } }
        paths p = map paramPath (validatePatch p)
    in [ ("Presets validate", all (null . validatePatch) (Map.elems presetPatches))
       , ("Out-of-range values report their field paths",
            paths badCutoff == ["filter.cutoff"]
            && paths noOscs == ["oscillators"]
            && paths badOsc == ["oscillators[0].level"]
            && paths badEffect == ["effects[0].feedback"]
            && paths nanSustain == ["amplitudeEnvelope.sustain"])
       , ("Unknown effect types are rejected when decoding",
            isLeft (eitherDecode "{\"type\":\"flanger\",\"rate\":1}" :: Either String EffectSpec))
       ]

-------------------------------------------------------------------------------
-- Projects

projectScenarios :: [(String, Bool)]
projectScenarios =
    let p0 = newProject baseScore
        lead0 = trackNamed "lead" p0
        bass0 = trackNamed "bass" p0
    in
    [ ("New project compiles with stable ids and no diagnostics",
        scoreIsCurrent p0 && null (projectDiagnostics p0) && projectRevision p0 == Revision 0
        && map trackId (projectTracks p0) == [TrackId "t1", TrackId "t2"])

    , ("Revision advances only on change", let
        p1 = run [UpdateTrackMix (trackId lead0) noMixUpdate { updateGain = Just 0.5 }] p0
        p2 = run [UpdateTrackMix (trackId lead0) noMixUpdate { updateGain = Just 0.5 }] p1
        p3 = run [SetScoreText baseScore] p2
        in projectRevision p1 == Revision 1 && projectRevision p2 == Revision 1 && projectRevision p3 == Revision 1)

    , ("Invalid commands are rejected without changing the project", let
        bad = (patchVoice somePatch) { voiceFilterRes = 99 }
        r1 = applyCommand (SetTrackPatch (trackId lead0) somePatch { patchVoice = bad }) p0
        r2 = applyCommand (UpdateTrackMix (TrackId "t99") noMixUpdate) p0
        r3 = applyCommand (UpdateTrackMix (trackId lead0) noMixUpdate { updatePan = Just 2 }) p0
        r4 = applyCommand (SetMasterGain (1 / 0)) p0
        code = either commandErrorCode (const "")
        in code r1 == "invalid_value" && map paramPath (either commandErrorParams (const []) r1) == ["filter.resonance"]
           && code r2 == "unknown_track" && code r3 == "invalid_value" && code r4 == "invalid_value")

    , ("Partial mix updates leave other fields alone", let
        p1 = run [ UpdateTrackMix (trackId bass0) noMixUpdate { updatePan = Just (-0.5) }
                 , UpdateTrackMix (trackId bass0) noMixUpdate { updateMute = Just True } ] p0
        in trackMix (trackNamed "bass" p1) == TrackMix 1 (-0.5) True False)

    , ("Invalid score text keeps the last valid score playable", let
        broken = baseScore <> "track oops (lead\n    C4 quarter\n"
        p1 = run [UpdateTrackMix (trackId lead0) noMixUpdate { updateSolo = Just True }, SetScoreText broken] p0
        diag = firstDiag p1
        fixed = run [SetScoreText baseScore] p1
        in projectScoreText p1 == broken && not (scoreIsCurrent p1)
           && fmap validText (projectValidScore p1) == Just baseScore
           && projectTracks p1 == projectTracks (run [UpdateTrackMix (trackId lead0) noMixUpdate { updateSolo = Just True }] p0)
           && diagSeverity diag == SeverityError && diagLine diag == Just 10 && diagColumn diag == Just 17
           && fmap compiledIsCurrent (compileProject p1) == Just False
           && scoreIsCurrent fixed && null (projectDiagnostics fixed))

    , ("Note edits and reordering keep track identity and settings", let
        p1 = run [SetTrackPatch (trackId lead0) somePatch] p0
        reordered = score [ "title \"Test\"", "tempo 100"
                          , "track bass (bass):", "    C2 whole"
                          , "track lead (pluck):", "    G4 eighth" ]
        p2 = run [SetScoreText reordered] p1
        lead2 = trackNamed "lead" p2
        in map trackScoreName (projectTracks p2) == ["bass", "lead"]
           && trackId lead2 == trackId lead0 && trackId (trackNamed "bass" p2) == trackId bass0
           && trackPatchOverride lead2 == Just somePatch && trackScoreInstrument lead2 == "pluck")

    , ("Removed tracks are retired and revived by name", let
        p1 = run [UpdateTrackMix (trackId bass0) noMixUpdate { updateGain = Just 0.25 }] p0
        withoutBass = score [ "track lead (lead):", "    C4 quarter" ]
        p2 = run [SetScoreText withoutBass] p1
        p3 = run [SetScoreText baseScore] p2
        in map trackScoreName (projectTracks p2) == ["lead"]
           && map retiredId (projectRetired p2) == [trackId bass0]
           && trackId (trackNamed "bass" p3) == trackId bass0
           && mixGain (trackMix (trackNamed "bass" p3)) == 0.25
           && null (projectRetired p3))

    , ("Renames are not guessed; settings move only when asked", let
        p1 = run [SetTrackPatch (trackId lead0) somePatch] p0
        renamed = T.replace "track lead (lead)" "track melody (lead)" baseScore
        p2 = run [SetScoreText renamed] p1
        melody = trackNamed "melody" p2
        p3 = run [AdoptRetiredSettings (trackId lead0) (trackId melody)] p2
        in trackId melody == TrackId "t3" && trackPatchOverride melody == Nothing
           && trackPatchOverride (trackNamed "melody" p3) == Just somePatch
           && null (projectRetired p3)
           && isLeft (applyCommand (AdoptRetiredSettings (trackId lead0) (trackId melody)) p3))

    , ("Duplicate track names are errors and change nothing playable", let
        dup = baseScore <> "\ntrack lead (pad):\n    C3 whole\n"
        p1 = run [SetScoreText dup] p0
        d = firstDiag p1
        in projectTracks p1 == projectTracks p0 && not (scoreIsCurrent p1)
           && diagSeverity d == SeverityError && diagLine d == Just 11)

    , ("Unknown instruments warn and fall back to the default patch", let
        p1 = newProject (score ["track x (theremin):", "    C4 quarter"])
        c = fromJust (compileProject p1)
        in scoreIsCurrent p1
           && map diagSeverity (projectDiagnostics p1) == [SeverityWarning]
           && map compiledPatchSource (compiledTracks c) == [FromDefault])

    , ("Patch precedence: override, then score instrument, then preset", let
        custom = score [ "instrument lead:", "    oscillator sine", "    cutoff 900"
                       , "track a (lead):", "    C4 quarter", "track b (bass):", "    C2 quarter" ]
        p1 = newProject custom
        tA = trackId (trackNamed "a" p1)
        sources p = map compiledPatchSource (compiledTracks (fromJust (compileProject p)))
        p2 = run [SetTrackPatch tA somePatch] p1
        p3 = run [ResetTrackPatch tA] p2
        cutoffOf p = voiceFilterCutoff (patchVoice (compiledPatch (firstTrack (fromJust (compileProject p)))))
        in sources p1 == [FromScoreInstrument, FromPreset] && cutoffOf p1 == 900
           && sources p2 == [FromOverride, FromPreset] && cutoffOf p2 == voiceFilterCutoff (patchVoice somePatch)
           && sources p3 == [FromScoreInstrument, FromPreset])

    , ("Project and command JSON round trips", let
        p1 = run [ SetTrackPatch (trackId lead0) somePatch
                 , UpdateTrackMix (trackId bass0) noMixUpdate { updateMute = Just True }
                 , SetMasterGain 0.8
                 , SetScoreText (baseScore <> "track broken (\n") ] p0
        cmds = [ SetScoreText "x", SetTrackPatch (TrackId "t1") somePatch, ResetTrackPatch (TrackId "t2")
               , UpdateTrackMix (TrackId "t1") noMixUpdate { updatePan = Just 0.1, updateSolo = Just False }
               , SetMasterGain 1.5, AdoptRetiredSettings (TrackId "t3") (TrackId "t4") ]
        in decode (encode p1) == Just p1 && decode (encode cmds) == Just cmds)
    ]

-- | A project built without overrides sounds exactly like rendering the score directly.
scoreParity :: IO Bool
scoreParity = do
    results <- forM ["examples/synthwave.sonora", "examples/bach_prelude.sonora", "examples/ambient.sonora"] $ \path -> do
        txt <- TIO.readFile path
        let p = newProject txt
            viaProject = runSignal (renderSong (compiledSong (fromJust (compileProject p)))) testCfg
        return $ case parseSonora path (T.unpack txt) of
            Left _ -> False
            Right ast -> scoreIsCurrent p && runSignal (renderSong (evaluateSong ast)) testCfg == viaProject
    return (and results)

-- | Random command sequences preserve the track invariants.
prop_commandInvariants :: Property
prop_commandInvariants = forAll (listOf genCommand) $ \cmds ->
    let step p c = either (const p) id (applyCommand c p)
        ps = scanl step (newProject baseScore) cmds
        invariant p =
            let ids = map trackId (projectTracks p) ++ map retiredId (projectRetired p)
                names = maybe [] (map (T.pack . astTrackName) . astSongTracks . validAst) (projectValidScore p)
            in map trackScoreName (projectTracks p) == names
               && length ids == length (Map.fromList [ (i, ()) | i <- ids ])
               && length (projectRetired p) <= maxRetiredTracks
        revisions = map (revisionNumber . projectRevision) ps
    in all invariant ps && and (zipWith (<=) revisions (drop 1 revisions))
  where
    trackNames = ["lead", "bass", "pad", "keys"]
    genScore = do
        names <- sublistOf trackNames >>= shuffle
        broken <- frequency [(4, return False), (1, return True)]
        return $ score ([ "track " <> n <> " (pluck):\n    C4 quarter" | n <- names ] ++ [ "track (" | broken ])
    genTrackId = TrackId . ("t" <>) . T.pack . show <$> choose (1 :: Int, 12)
    genCommand = oneof
        [ SetScoreText <$> genScore
        , SetTrackPatch <$> genTrackId <*> pure somePatch
        , ResetTrackPatch <$> genTrackId
        , UpdateTrackMix <$> genTrackId <*> (MixUpdate <$> liftArbitrary (choose (0, 2)) <*> pure Nothing <*> arbitrary <*> pure Nothing)
        , AdoptRetiredSettings <$> genTrackId <*> genTrackId ]

projectTests :: IO Bool
projectTests = do
    putStrLn "--- Running Project Model Tests ---"
    p1 <- check "Preset patches render bit-exact to built-in instruments" presetsBitExact
    r1 <- quickCheckResult (withNumTests 200 prop_patchJsonRoundTrip)
    vs <- mapM (uncurry check) validationScenarios
    ps <- mapM (uncurry check) projectScenarios
    parity <- scoreParity >>= check "Projects without overrides render exactly like their scores"
    r2 <- quickCheckResult (withNumTests 300 prop_commandInvariants)
    return (p1 && isSuccess r1 && and vs && and ps && parity && isSuccess r2)
