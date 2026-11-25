{-# LANGUAGE OverloadedStrings #-}
module AuditionSpec (auditionTests, editablePerturbations) where

import Control.Exception (SomeException, catch, displayException)
import Data.Text (Text)
import qualified Data.Vector.Storable as V

import Sonora.Audio.Audition
import Sonora.Signal.Core (Frame (..))
import Sonora.Signal.Envelope (ADSR (..), EnvelopeCurve (..))
import Sonora.Signal.Filter (FilterType (..))
import Sonora.Signal.Oscillator (Waveform (..))
import Sonora.Synth.Patch
import Sonora.Synth.Voice

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

sr :: Int
sr = 8000

-- | A sound using every kind of control the editor offers.
base :: Patch
base = Patch "test" VoiceConfig
    { voiceOscillators =
        [ defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.6, oscSetFineCent = -5 }
        , defaultOscSetting { oscSetWaveform = Square 0.3, oscSetMix = 0.4, oscSetSemitone = 7 }
        ]
    , voiceFilterType = LowPass
    , voiceFilterCutoff = 1200
    , voiceFilterRes = 2
    , voiceFilterEnvAmt = 1500
    , voiceFilterEnv = ADSR 0.05 0.2 0.4 0.3 LinearCurve
    , voiceAmpEnv = ADSR 0.02 0.15 0.7 0.25 LinearCurve
    }
    [ TremoloEffect 5 0.4, DistortionEffect 2, ChorusEffect 1.2 0.4 0.3, DelayEffect 0.15 0.3 0.3, ReverbEffect 0.5 0.3 ]

note :: AuditionNote
note = AuditionNote 57 0.8 0.6

render :: Patch -> V.Vector Frame
render p = renderAudition sr p note

different :: V.Vector Frame -> V.Vector Frame -> Bool
different a b = V.length a /= V.length b || V.any (> 1e-4) (V.zipWith (\(Frame l r) (Frame l' r') -> max (abs (l - l')) (abs (r - r'))) a b)

voice :: (VoiceConfig -> VoiceConfig) -> Patch -> Patch
voice f p = p { patchVoice = f (patchVoice p) }

osc :: Int -> (OscSetting -> OscSetting) -> Patch -> Patch
osc i f = voice $ \v -> v { voiceOscillators = [ if k == i then f o else o | (k, o) <- zip [0 ..] (voiceOscillators v) ] }

effect :: Int -> (EffectSpec -> EffectSpec) -> Patch -> Patch
effect i f p = p { patchEffects = [ if k == i then f e else e | (k, e) <- zip [0 ..] (patchEffects p) ] }

env :: (ADSR -> ADSR) -> (VoiceConfig -> ADSR) -> (VoiceConfig -> ADSR -> VoiceConfig) -> Patch -> Patch
env f get set = voice (\v -> set v (f (get v)))

amp, flt :: (ADSR -> ADSR) -> Patch -> Patch
amp f = env f voiceAmpEnv (\v e -> v { voiceAmpEnv = e })
flt f = env f voiceFilterEnv (\v e -> v { voiceFilterEnv = e })

-- | Every value the sound editor can change, each changed once.
editablePerturbations :: [(Text, Patch -> Patch)]
editablePerturbations =
    [ ("oscillator waveform", osc 0 (\o -> o { oscSetWaveform = Triangle }))
    , ("oscillator level", osc 0 (\o -> o { oscSetMix = 0.3 }))
    , ("oscillator semitones", osc 1 (\o -> o { oscSetSemitone = 5 }))
    , ("oscillator cents", osc 0 (\o -> o { oscSetFineCent = 20 }))
    , ("square duty", osc 1 (\o -> o { oscSetWaveform = Square 0.6 }))
    , ("add an oscillator", voice (\v -> v { voiceOscillators = voiceOscillators v ++ [defaultOscSetting { oscSetWaveform = Sine, oscSetMix = 0.5, oscSetSemitone = 12 }] }))
    , ("remove an oscillator", voice (\v -> v { voiceOscillators = take 1 (voiceOscillators v) }))
    , ("filter mode", voice (\v -> v { voiceFilterType = HighPass }))
    , ("filter cutoff", voice (\v -> v { voiceFilterCutoff = 600 }))
    , ("filter resonance", voice (\v -> v { voiceFilterRes = 6 }))
    , ("filter sweep", voice (\v -> v { voiceFilterEnvAmt = -500 }))
    , ("filter attack", flt (\e -> e { adsrAttack = 0.2 }))
    , ("filter decay", flt (\e -> e { adsrDecay = 0.05 }))
    , ("filter sustain", flt (\e -> e { adsrSustain = 0.9 }))
    , ("filter release", flt (\e -> e { adsrRelease = 0.05 }))
    , ("filter curve", flt (\e -> e { adsrCurve = ExponentialCurve }))
    , ("attack", amp (\e -> e { adsrAttack = 0.1 }))
    , ("decay", amp (\e -> e { adsrDecay = 0.4 }))
    , ("sustain", amp (\e -> e { adsrSustain = 0.3 }))
    , ("release", amp (\e -> e { adsrRelease = 0.6 }))
    , ("loudness curve", amp (\e -> e { adsrCurve = ExponentialCurve }))
    , ("tremolo rate", effect 0 (\_ -> TremoloEffect 9 0.4))
    , ("tremolo depth", effect 0 (\_ -> TremoloEffect 5 0.8))
    , ("distortion drive", effect 1 (\_ -> DistortionEffect 6))
    , ("chorus rate", effect 2 (\_ -> ChorusEffect 3 0.4 0.3))
    , ("chorus depth", effect 2 (\_ -> ChorusEffect 1.2 0.9 0.3))
    , ("chorus mix", effect 2 (\_ -> ChorusEffect 1.2 0.4 0.7))
    , ("delay time", effect 3 (\_ -> DelayEffect 0.3 0.3 0.3))
    , ("delay feedback", effect 3 (\_ -> DelayEffect 0.15 0.7 0.3))
    , ("delay mix", effect 3 (\_ -> DelayEffect 0.15 0.3 0.6))
    , ("reverb room", effect 4 (\_ -> ReverbEffect 0.9 0.3))
    , ("reverb mix", effect 4 (\_ -> ReverbEffect 0.5 0.7))
    , ("effect order", \p -> p { patchEffects = reverse (patchEffects p) })
    , ("remove an effect", \p -> p { patchEffects = drop 1 (patchEffects p) })
    , ("add an effect", \p -> p { patchEffects = patchEffects p ++ [DelayEffect 0.05 0.2 0.2] })
    ]

rms :: V.Vector Frame -> Double
rms v = sqrt (V.sum (V.map (\(Frame l r) -> l * l + r * r) v) / fromIntegral (max 1 (V.length v)))

-- | Mean absolute sample-to-sample change: grows with high-frequency content.
roughness :: V.Vector Frame -> Double
roughness v = V.sum (V.zipWith (\(Frame a _) (Frame b _) -> abs (a - b)) v (V.drop 1 v)) / fromIntegral (max 1 (V.length v))

auditionTests :: IO Bool
auditionTests = do
    putStrLn "\n--- Audition rendering ---"
    let reference = render base
        silent = [ name | (name, f) <- editablePerturbations, not (different reference (render (f base))) ]
    rs <- sequence
        [ check "the test sound is valid" $ return (null (validatePatch base))
        , check ("every editable value changes the sound (" ++ show (length editablePerturbations) ++ " values)") $ do
            mapM_ (\n -> putStrLn ("    no audible change: " ++ show n)) silent
            return (null silent && all (null . validatePatch . ($ base) . snd) editablePerturbations)
        , check "noise has no pitch, so the editor can disable pitch for it" $ do
            let noisy = osc 0 (\o -> o { oscSetWaveform = WhiteNoiseGen }) (voice (\v -> v { voiceOscillators = take 1 (voiceOscillators v) }) base { patchEffects = [] })
            return (not (different (render noisy) (render (osc 0 (\o -> o { oscSetSemitone = 7, oscSetFineCent = 30 }) noisy))))
        , check "changes move the sound the right way" $ do
            let dry = base { patchEffects = [] }
                darker = voice (\v -> v { voiceFilterCutoff = 300, voiceFilterEnvAmt = 0 }) dry
                brighter = voice (\v -> v { voiceFilterCutoff = 6000, voiceFilterEnvAmt = 0 }) dry
                quieter = voice (\v -> v { voiceOscillators = [ o { oscSetMix = oscSetMix o / 3 } | o <- voiceOscillators v ] }) dry
                longer = amp (\e -> e { adsrRelease = 1.5 }) dry
            return (roughness (render darker) < roughness (render brighter)
                    && rms (render quieter) < rms (render dry)
                    && V.length (render longer) > V.length (render dry))
        , check "a released note matches the held note up to the release" $ do
            let held = renderAudition sr base note { noteGate = holdSeconds }
                released = renderAudition sr base note { noteGate = 0.5 }
                upTo = round (0.5 * fromIntegral sr :: Double)
            return (V.take upTo held == V.take upTo released && V.length released < V.length held)
        , check "the inaudible end of a tail is trimmed" $ do
            let v = render base
                Frame l r = V.last v
            return (abs l >= 1e-4 || abs r >= 1e-4)
        ]
    return (and rs)
