{-# LANGUAGE DeriveGeneric #-}
module Sonora.Synth.Instrument
    ( Instrument (..)
    , defaultInstrument
    , defaultInstruments
    , lookupInstrument
    -- Specific presets
    , leadSynth
    , bassSynth
    , padSynth
    , pluckSynth
    , chimeSynth
    , pianoSynth
    ) where

import qualified Data.Map.Strict as Map
import Sonora.Signal.Core
import Sonora.Signal.Effects
import Sonora.Signal.Envelope
import Sonora.Signal.Filter
import Sonora.Signal.Oscillator
import Sonora.Synth.Voice

-- | An instrument pairs a synth voice configuration with an effect chain.
data Instrument = Instrument
    { instName    :: !String
    , instVoice   :: !VoiceConfig
    , instEffects :: ![Signal -> Signal]
    }

defaultInstrument :: Instrument
defaultInstrument = pianoSynth

-- | Warm acoustic-like piano preset with multi-harmonic sustain and gentle decay.
pianoSynth :: Instrument
pianoSynth = Instrument
    { instName  = "piano"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Triangle, oscSetMix = 0.8 }
            , defaultOscSetting { oscSetWaveform = Sine, oscSetMix = 0.4, oscSetSemitone = 12 }
            , defaultOscSetting { oscSetWaveform = Sine, oscSetMix = 0.2, oscSetSemitone = 19 }
            ]
        , voiceFilterType   = LowPass
        , voiceFilterCutoff = 3500.0
        , voiceFilterRes    = 0.7
        , voiceFilterEnvAmt = 2000.0
        , voiceFilterEnv    = adsr 0.005 0.3 0.2 0.3
        , voiceAmpEnv       = adsr 0.008 0.4 0.3 0.4
        }
    , instEffects = [simpleReverb 0.25]
    }

-- | Punchy, modern synth lead with subtle chorus and stereo delay.
leadSynth :: Instrument
leadSynth = Instrument
    { instName  = "lead"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.7, oscSetFineCent = -7.0 }
            , defaultOscSetting { oscSetWaveform = Square 0.5, oscSetMix = 0.6, oscSetFineCent = 7.0 }
            , defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.4, oscSetSemitone = -12 }
            ]
        , voiceFilterType   = LowPass
        , voiceFilterCutoff = 3000.0
        , voiceFilterRes    = 1.5
        , voiceFilterEnvAmt = 3500.0
        , voiceFilterEnv    = adsr 0.02 0.2 0.4 0.2
        , voiceAmpEnv       = adsr 0.01 0.1 0.7 0.2
        }
    , instEffects =
        [ chorus 1.5 0.3 0.2
        , delay 0.25 0.35 0.25
        , simpleReverb 0.20
        ]
    }

-- | Fat, driving analog synth bass.
bassSynth :: Instrument
bassSynth = Instrument
    { instName  = "bass"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.8 }
            , defaultOscSetting { oscSetWaveform = Square 0.5, oscSetMix = 0.7, oscSetSemitone = -12 }
            , defaultOscSetting { oscSetWaveform = Triangle, oscSetMix = 0.5 }
            ]
        , voiceFilterType   = LowPass
        , voiceFilterCutoff = 800.0
        , voiceFilterRes    = 2.2
        , voiceFilterEnvAmt = 2200.0
        , voiceFilterEnv    = adsr 0.005 0.15 0.2 0.1
        , voiceAmpEnv       = adsr 0.005 0.12 0.8 0.1
        }
    , instEffects = [distortion 1.4]
    }

-- | Lush, wide ambient pad with slow attack and shimmering reverb.
padSynth :: Instrument
padSynth = Instrument
    { instName  = "pad"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.5, oscSetFineCent = -10.0 }
            , defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.5, oscSetFineCent = 10.0 }
            , defaultOscSetting { oscSetWaveform = Triangle, oscSetMix = 0.6, oscSetSemitone = 12 }
            ]
        , voiceFilterType   = LowPass
        , voiceFilterCutoff = 1800.0
        , voiceFilterRes    = 0.8
        , voiceFilterEnvAmt = 1500.0
        , voiceFilterEnv    = adsr 0.6 0.8 0.7 1.0
        , voiceAmpEnv       = adsr 0.4 0.6 0.8 0.8
        }
    , instEffects =
        [ chorus 0.8 0.5 0.4
        , schroederReverb 0.8 0.45
        ]
    }

-- | Crisp, resonant plucked instrument (guitar / harp / synth pluck).
pluckSynth :: Instrument
pluckSynth = Instrument
    { instName  = "pluck"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Square 0.25, oscSetMix = 0.7 }
            , defaultOscSetting { oscSetWaveform = Triangle, oscSetMix = 0.6, oscSetSemitone = 12 }
            ]
        , voiceFilterType   = LowPass
        , voiceFilterCutoff = 1200.0
        , voiceFilterRes    = 2.5
        , voiceFilterEnvAmt = 4000.0
        , voiceFilterEnv    = adsr 0.001 0.12 0.0 0.10
        , voiceAmpEnv       = adsr 0.001 0.15 0.0 0.12
        }
    , instEffects =
        [ delay 0.20 0.30 0.25
        , simpleReverb 0.25
        ]
    }

-- | Sparkling bell / chime preset with inharmonic overtone ratios.
chimeSynth :: Instrument
chimeSynth = Instrument
    { instName  = "chime"
    , instVoice = VoiceConfig
        { voiceOscillators  =
            [ defaultOscSetting { oscSetWaveform = Sine, oscSetMix = 0.7 }
            , defaultOscSetting { oscSetWaveform = Sine, oscSetMix = 0.5, oscSetSemitone = 19, oscSetFineCent = 15.0 }
            , defaultOscSetting { oscSetWaveform = Triangle, oscSetMix = 0.3, oscSetSemitone = 28 }
            ]
        , voiceFilterType   = HighPass
        , voiceFilterCutoff = 400.0
        , voiceFilterRes    = 1.0
        , voiceFilterEnvAmt = 0.0
        , voiceFilterEnv    = adsr 0.001 0.5 0.0 0.5
        , voiceAmpEnv       = adsr 0.002 0.8 0.0 0.6
        }
    , instEffects =
        [ chorus 1.2 0.4 0.3
        , delay 0.33 0.4 0.3
        , schroederReverb 0.7 0.4
        ]
    }

-- | Built-in instrument registry.
defaultInstruments :: Map.Map String Instrument
defaultInstruments = Map.fromList
    [ ("piano", pianoSynth)
    , ("lead",  leadSynth)
    , ("bass",  bassSynth)
    , ("pad",   padSynth)
    , ("pluck", pluckSynth)
    , ("chime", chimeSynth)
    ]

-- | Find an instrument by name (case-insensitive).
lookupInstrument :: String -> Maybe Instrument
lookupInstrument name = Map.lookup name defaultInstruments
