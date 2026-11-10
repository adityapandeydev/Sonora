{-# LANGUAGE OverloadedStrings #-}
-- | Serializable instrument patches.
--
-- An 'Instrument' carries its effect chain as functions, which cannot be
-- stored, compared, or sent over the wire. A 'Patch' describes the same
-- instrument as data: the existing 'VoiceConfig' plus a list of
-- 'EffectSpec's, which 'compilePatch' turns back into an 'Instrument'.
--
-- JSON layout:
--
-- > { "name": "lead",
-- >   "oscillators": [ { "waveform": {"type": "saw"}, "level": 0.7,
-- >                      "semitones": 0, "cents": -7, "phase": 0 } ],
-- >   "filter": { "mode": "lowpass", "cutoff": 3000, "resonance": 1.5,
-- >               "envelopeAmount": 3500, "envelope": { ...envelope... } },
-- >   "amplitudeEnvelope": { "attack": 0.01, "decay": 0.1, "sustain": 0.7,
-- >                          "release": 0.2, "curve": "linear" },
-- >   "effects": [ { "type": "delay", "time": 0.25, "feedback": 0.35, "mix": 0.25 } ] }
module Sonora.Synth.Patch
    ( Patch (..)
    , EffectSpec (..)
    , compilePatch
    , compileEffect
      -- * Validation
    , ParamError (..)
    , validatePatch
    , maxOscillators
    , maxEffects
      -- * Presets
    , presetPatches
    , lookupPresetPatch
    , defaultPatch
    , patchFromInstrumentDef
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Sonora.DSL.AST (ASTInstrumentDef (..))
import Sonora.Signal.Core (Signal)
import Sonora.Signal.Effects
import Sonora.Signal.Envelope
import Sonora.Signal.Filter (FilterType (..))
import Sonora.Signal.Oscillator (Waveform (..))
import Sonora.Synth.Instrument
import Sonora.Synth.Voice

-- | One stage of an instrument's effect chain, applied in list order.
data EffectSpec
    = DelayEffect !Double !Double !Double
      -- ^ Time in seconds, feedback (0 to 0.95), wet mix (0 to 1).
    | ReverbEffect !Double !Double
      -- ^ Room size (0 to 1), wet mix (0 to 1).
    | ChorusEffect !Double !Double !Double
      -- ^ LFO rate in Hz, depth (0 to 1), wet mix (0 to 1).
    | DistortionEffect !Double
      -- ^ Drive, 1 (clean) and up.
    | TremoloEffect !Double !Double
      -- ^ LFO rate in Hz, depth (0 to 1).
    deriving (Eq, Show)

-- | A complete, serializable instrument description.
data Patch = Patch
    { patchName    :: !Text
    , patchVoice   :: !VoiceConfig
    , patchEffects :: ![EffectSpec]
    } deriving (Eq, Show)

compileEffect :: EffectSpec -> Signal -> Signal
compileEffect e = case e of
    DelayEffect t fb m    -> delay t fb m
    ReverbEffect room m   -> schroederReverb room m
    ChorusEffect r d m    -> chorus r d m
    DistortionEffect d    -> distortion d
    TremoloEffect r d     -> tremolo r d

-- | Build the playable instrument described by a patch.
compilePatch :: Patch -> Instrument
compilePatch (Patch name voice effects) = Instrument
    { instName    = T.unpack name
    , instVoice   = voice
    , instEffects = map compileEffect effects
    }

-------------------------------------------------------------------------------
-- Validation

-- | A parameter outside its accepted range. The path names the field in the
-- JSON layout, e.g. @oscillators[1].level@.
data ParamError = ParamError
    { paramPath    :: !Text
    , paramMessage :: !Text
    } deriving (Eq, Show)

instance ToJSON ParamError where
    toJSON (ParamError p m) = object ["path" .= p, "message" .= m]

maxOscillators, maxEffects :: Int
maxOscillators = 8
maxEffects = 8

-- | All range violations in a patch; empty when the patch is acceptable.
validatePatch :: Patch -> [ParamError]
validatePatch (Patch name VoiceConfig{..} effects) = concat
    [ [ ParamError "name" "must not be empty" | T.null (T.strip name) ]
    , [ ParamError "oscillators" "must contain between 1 and 8 oscillators"
      | null voiceOscillators || length voiceOscillators > maxOscillators ]
    , concat [ oscErrors ("oscillators[" <> showT i <> "]") o | (i, o) <- zip [0 :: Int ..] voiceOscillators ]
    , range "filter.cutoff" 20 20000 voiceFilterCutoff
    , range "filter.resonance" 0.1 20 voiceFilterRes
    , range "filter.envelopeAmount" (-20000) 20000 voiceFilterEnvAmt
    , envErrors "filter.envelope" voiceFilterEnv
    , envErrors "amplitudeEnvelope" voiceAmpEnv
    , [ ParamError "effects" "must contain at most 8 effects" | length effects > maxEffects ]
    , concat [ effectErrors ("effects[" <> showT i <> "]") e | (i, e) <- zip [0 :: Int ..] effects ]
    ]
  where
    oscErrors p OscSetting{..} = concat
        [ case oscSetWaveform of
            Square duty -> range (p <> ".waveform.duty") 0.01 0.99 duty
            _           -> []
        , range (p <> ".level") 0 1 oscSetMix
        , range (p <> ".semitones") (-48) 48 (fromIntegral oscSetSemitone)
        , range (p <> ".cents") (-100) 100 oscSetFineCent
        , range (p <> ".phase") 0 (2 * pi) oscSetPhase
        ]
    envErrors p ADSR{..} = concat
        [ range (p <> ".attack") 0 20 adsrAttack
        , range (p <> ".decay") 0 20 adsrDecay
        , range (p <> ".sustain") 0 1 adsrSustain
        , range (p <> ".release") 0 20 adsrRelease
        ]
    effectErrors p e = case e of
        DelayEffect t fb m -> range (p <> ".time") 0.001 4 t ++ range (p <> ".feedback") 0 0.95 fb ++ range (p <> ".mix") 0 1 m
        ReverbEffect r m   -> range (p <> ".roomSize") 0 1 r ++ range (p <> ".mix") 0 1 m
        ChorusEffect r d m -> range (p <> ".rate") 0.01 10 r ++ range (p <> ".depth") 0 1 d ++ range (p <> ".mix") 0 1 m
        DistortionEffect d -> range (p <> ".drive") 1 50 d
        TremoloEffect r d  -> range (p <> ".rate") 0.01 20 r ++ range (p <> ".depth") 0 1 d

range :: Text -> Double -> Double -> Double -> [ParamError]
range path lo hi x
    | isNaN x || isInfinite x = [ParamError path "must be a finite number"]
    | x < lo || x > hi = [ParamError path ("must be between " <> showNum lo <> " and " <> showNum hi)]
    | otherwise = []

showNum :: Double -> Text
showNum x
    | x == fromIntegral (round x :: Integer) = showT (round x :: Integer)
    | otherwise = showT x

showT :: Show a => a -> Text
showT = T.pack . show

-------------------------------------------------------------------------------
-- Presets
--
-- Voices come straight from the built-in instruments; effect chains are the
-- same calls expressed as data (checked bit-exact by the test suite).

presetPatches :: Map.Map Text Patch
presetPatches = Map.fromList [ (patchName p, p) | p <- presets ]
  where
    preset inst = Patch (T.pack (instName inst)) (instVoice inst)
    presets =
        [ preset pianoSynth [ReverbEffect 0.5 0.25]
        , preset leadSynth [ChorusEffect 1.5 0.3 0.2, DelayEffect 0.25 0.35 0.25, ReverbEffect 0.5 0.20]
        , preset bassSynth [DistortionEffect 1.4]
        , preset padSynth [ChorusEffect 0.8 0.5 0.4, ReverbEffect 0.8 0.45]
        , preset pluckSynth [DelayEffect 0.20 0.30 0.25, ReverbEffect 0.5 0.25]
        , preset chimeSynth [ChorusEffect 1.2 0.4 0.3, DelayEffect 0.33 0.4 0.3, ReverbEffect 0.7 0.4]
        ]

lookupPresetPatch :: Text -> Maybe Patch
lookupPresetPatch name = Map.lookup name presetPatches

-- | The patch used when a score names no known instrument (as the DSL evaluator does).
defaultPatch :: Patch
defaultPatch = presetPatches Map.! T.pack (instName defaultInstrument)

-- | The patch described by an @instrument@ declaration in a score, built
-- exactly as the DSL evaluator builds its instrument.
patchFromInstrumentDef :: ASTInstrumentDef -> Patch
patchFromInstrumentDef ASTInstrumentDef{..} =
    let wave = case astInstOsc of
            "sine"     -> Sine
            "square"   -> Square 0.5
            "triangle" -> Triangle
            _          -> Sawtooth
        (a, d, s, r) = astInstADSR
        voice = VoiceConfig
            { voiceOscillators  = [defaultOscSetting { oscSetWaveform = wave }]
            , voiceFilterType   = LowPass
            , voiceFilterCutoff = astInstCutoff
            , voiceFilterRes    = astInstRes
            , voiceFilterEnvAmt = astInstCutoff * 0.5
            , voiceFilterEnv    = adsr a d s r
            , voiceAmpEnv       = adsr a d s r
            }
    in Patch (T.pack astInstName) voice []

-------------------------------------------------------------------------------
-- JSON

instance ToJSON Patch where
    toJSON (Patch name VoiceConfig{..} effects) = object
        [ "name"              .= name
        , "oscillators"       .= map oscToJSON voiceOscillators
        , "filter"            .= object
            [ "mode"           .= filterModeText voiceFilterType
            , "cutoff"         .= voiceFilterCutoff
            , "resonance"      .= voiceFilterRes
            , "envelopeAmount" .= voiceFilterEnvAmt
            , "envelope"       .= envToJSON voiceFilterEnv
            ]
        , "amplitudeEnvelope" .= envToJSON voiceAmpEnv
        , "effects"           .= effects
        ]

instance FromJSON Patch where
    parseJSON = withObject "Patch" $ \o -> do
        name <- o .: "name"
        oscs <- o .: "oscillators" >>= mapM oscFromJSON
        flt <- o .: "filter"
        (mode, cutoff, res, envAmt, fenv) <- withObject "filter" (\f -> (,,,,)
            <$> (f .: "mode" >>= filterModeFromText)
            <*> f .: "cutoff"
            <*> f .: "resonance"
            <*> f .: "envelopeAmount"
            <*> (f .: "envelope" >>= envFromJSON)) flt
        aenv <- o .: "amplitudeEnvelope" >>= envFromJSON
        effects <- o .:? "effects" .!= []
        return $ Patch name VoiceConfig
            { voiceOscillators  = oscs
            , voiceFilterType   = mode
            , voiceFilterCutoff = cutoff
            , voiceFilterRes    = res
            , voiceFilterEnvAmt = envAmt
            , voiceFilterEnv    = fenv
            , voiceAmpEnv       = aenv
            } effects

oscToJSON :: OscSetting -> Value
oscToJSON OscSetting{..} = object
    [ "waveform"  .= waveToJSON oscSetWaveform
    , "level"     .= oscSetMix
    , "semitones" .= oscSetSemitone
    , "cents"     .= oscSetFineCent
    , "phase"     .= oscSetPhase
    ]

oscFromJSON :: Value -> Parser OscSetting
oscFromJSON = withObject "Oscillator" $ \o -> OscSetting
    <$> (o .: "waveform" >>= waveFromJSON)
    <*> o .: "level"
    <*> o .:? "semitones" .!= 0
    <*> o .:? "cents" .!= 0
    <*> o .:? "phase" .!= 0

waveToJSON :: Waveform -> Value
waveToJSON w = case w of
    Sine          -> object ["type" .= ("sine" :: Text)]
    Square duty   -> object ["type" .= ("square" :: Text), "duty" .= duty]
    Triangle      -> object ["type" .= ("triangle" :: Text)]
    Sawtooth      -> object ["type" .= ("saw" :: Text)]
    WhiteNoiseGen -> object ["type" .= ("whiteNoise" :: Text)]
    PinkNoiseGen  -> object ["type" .= ("pinkNoise" :: Text)]

waveFromJSON :: Value -> Parser Waveform
waveFromJSON = withObject "Waveform" $ \o -> do
    t <- o .: "type"
    case t :: Text of
        "sine"       -> return Sine
        "square"     -> Square <$> o .:? "duty" .!= 0.5
        "triangle"   -> return Triangle
        "saw"        -> return Sawtooth
        "whiteNoise" -> return WhiteNoiseGen
        "pinkNoise"  -> return PinkNoiseGen
        other        -> fail ("unknown waveform type " ++ show other)

envToJSON :: ADSR -> Value
envToJSON ADSR{..} = object
    [ "attack"  .= adsrAttack
    , "decay"   .= adsrDecay
    , "sustain" .= adsrSustain
    , "release" .= adsrRelease
    , "curve"   .= (case adsrCurve of LinearCurve -> "linear"; ExponentialCurve -> "exponential" :: Text)
    ]

envFromJSON :: Value -> Parser ADSR
envFromJSON = withObject "Envelope" $ \o -> do
    curve <- o .:? "curve" .!= "linear"
    c <- case curve :: Text of
        "linear"      -> return LinearCurve
        "exponential" -> return ExponentialCurve
        other         -> fail ("unknown envelope curve " ++ show other)
    ADSR <$> o .: "attack" <*> o .: "decay" <*> o .: "sustain" <*> o .: "release" <*> pure c

filterModeText :: FilterType -> Text
filterModeText t = case t of
    LowPass  -> "lowpass"
    HighPass -> "highpass"
    BandPass -> "bandpass"
    Notch    -> "notch"

filterModeFromText :: Text -> Parser FilterType
filterModeFromText t = case t of
    "lowpass"  -> return LowPass
    "highpass" -> return HighPass
    "bandpass" -> return BandPass
    "notch"    -> return Notch
    other      -> fail ("unknown filter mode " ++ show other)

instance ToJSON EffectSpec where
    toJSON e = object $ case e of
        DelayEffect t fb m -> ["type" .= ("delay" :: Text), "time" .= t, "feedback" .= fb, "mix" .= m]
        ReverbEffect r m   -> ["type" .= ("reverb" :: Text), "roomSize" .= r, "mix" .= m]
        ChorusEffect r d m -> ["type" .= ("chorus" :: Text), "rate" .= r, "depth" .= d, "mix" .= m]
        DistortionEffect d -> ["type" .= ("distortion" :: Text), "drive" .= d]
        TremoloEffect r d  -> ["type" .= ("tremolo" :: Text), "rate" .= r, "depth" .= d]

instance FromJSON EffectSpec where
    parseJSON = withObject "Effect" $ \o -> do
        t <- o .: "type"
        e <- case t :: Text of
            "delay"      -> DelayEffect <$> o .: "time" <*> o .: "feedback" <*> o .: "mix"
            "reverb"     -> ReverbEffect <$> o .: "roomSize" <*> o .: "mix"
            "chorus"     -> ChorusEffect <$> o .: "rate" <*> o .: "depth" <*> o .: "mix"
            "distortion" -> DistortionEffect <$> o .: "drive"
            "tremolo"    -> TremoloEffect <$> o .: "rate" <*> o .: "depth"
            other        -> fail ("unknown effect type " ++ show other)
        return e
