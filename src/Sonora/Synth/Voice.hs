{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Sonora.Synth.Voice
    ( OscSetting (..)
    , defaultOscSetting
    , VoiceConfig (..)
    , defaultVoiceConfig
    , renderVoice
    ) where

import GHC.Generics (Generic)
import qualified Data.Vector.Storable as V
import Sonora.Signal.Core
import Sonora.Signal.Envelope
import Sonora.Signal.Filter
import Sonora.Signal.Oscillator

-- | Settings for an individual oscillator inside a voice.
data OscSetting = OscSetting
    { oscSetWaveform :: !Waveform
    , oscSetMix      :: !Double    -- 0.0 to 1.0 relative level
    , oscSetSemitone :: !Int       -- Coarse pitch transpose (-24 to +24)
    , oscSetFineCent :: !Double    -- Fine detune in cents (-50 to +50)
    , oscSetPhase    :: !Double    -- Initial phase (0 to 2*pi)
    } deriving (Eq, Show, Generic)

defaultOscSetting :: OscSetting
defaultOscSetting = OscSetting
    { oscSetWaveform = Sawtooth
    , oscSetMix      = 1.0
    , oscSetSemitone = 0
    , oscSetFineCent = 0.0
    , oscSetPhase    = 0.0
    }

-- | Complete subtractive synth voice configuration.
data VoiceConfig = VoiceConfig
    { voiceOscillators   :: ![OscSetting]
    , voiceFilterType    :: !FilterType
    , voiceFilterCutoff  :: !Double   -- Base cutoff in Hz
    , voiceFilterRes     :: !Double   -- Resonance (Q)
    , voiceFilterEnvAmt  :: !Double   -- How much filter envelope sweeps cutoff (Hz)
    , voiceFilterEnv     :: !ADSR     -- Filter envelope
    , voiceAmpEnv        :: !ADSR     -- Amplitude envelope
    } deriving (Eq, Show, Generic)

defaultVoiceConfig :: VoiceConfig
defaultVoiceConfig = VoiceConfig
    { voiceOscillators  =
        [ defaultOscSetting { oscSetWaveform = Sawtooth, oscSetMix = 0.7, oscSetFineCent = -4.0 }
        , defaultOscSetting { oscSetWaveform = Square 0.5, oscSetMix = 0.5, oscSetSemitone = 0, oscSetFineCent = 4.0 }
        ]
    , voiceFilterType   = LowPass
    , voiceFilterCutoff = 2500.0
    , voiceFilterRes    = 1.2
    , voiceFilterEnvAmt = 3000.0
    , voiceFilterEnv    = adsr 0.05 0.20 0.30 0.20
    , voiceAmpEnv       = adsr 0.02 0.15 0.70 0.25
    }

-- | Synthesize a note voice given frequency, gate duration, and velocity.
renderVoice :: VoiceConfig -> Double -> Double -> Double -> Signal
renderVoice VoiceConfig{..} baseFreq gateDur velocity = Signal $ \cfg ->
    let totalDur = gateDur + adsrRelease voiceAmpEnv
        -- Calculate detuned frequency for an oscillator setting
        detunedFreq OscSetting{..} =
            let semitoneFactor = 2.0 ** (fromIntegral oscSetSemitone / 12.0)
                centFactor     = 2.0 ** (oscSetFineCent / 1200.0)
            in baseFreq * semitoneFactor * centFactor

        -- Generate and mix all oscillators
        renderOsc setting@OscSetting{..} =
            let freq = detunedFreq setting
                cfgOsc = OscillatorConfig
                    { oscWaveform  = oscSetWaveform
                    , oscFrequency = freq
                    , oscAmplitude = oscSetMix
                    , oscPhase     = oscSetPhase
                    , oscDuration  = totalDur
                    }
            in oscillator cfgOsc

        mixedOscSignal = case voiceOscillators of
            []     -> emptySignal
            [o]    -> renderOsc o
            (o:os) -> foldl mixSignals (renderOsc o) (map renderOsc os)

        -- Modulate filter cutoff with filter envelope
        cutoffVec = modulateParameter voiceFilterEnv gateDur voiceFilterCutoff
                        (voiceFilterCutoff + voiceFilterEnvAmt) cfg

        filteredSig = applyModulatedFilter voiceFilterType voiceFilterRes cutoffVec mixedOscSignal

        -- Modulate amplitude with amplitude envelope and note velocity
        ampVec = generateEnvelopeVector voiceAmpEnv gateDur cfg
        sigVec = runSignal filteredSig cfg
        len    = min (V.length sigVec) (V.length ampVec)

    in V.generate len $ \i ->
        let frame = sigVec V.! i
            envVal = ampVec V.! i
        in scaleFrame (envVal * velocity) frame
