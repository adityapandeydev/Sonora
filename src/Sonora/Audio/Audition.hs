-- | Single notes for auditioning a sound, rendered exactly as the song would
-- play them: the patch's voice, then its effect chain, then the limiter.
--
-- Samples before a note's gate do not depend on the gate's length (the
-- voice, filter and effects only look backwards in time), so a held note can
-- be released by rendering it again with a shorter gate and swapping the new
-- buffer in at the current position.
module Sonora.Audio.Audition
    ( AuditionNote (..)
    , renderAudition
    , auditionGain
    , holdSeconds
    , minPitch
    , maxPitch
    ) where

import qualified Data.Vector.Storable as V

import Sonora.Music.Pitch (midiToFreq)
import Sonora.Signal.Core
import Sonora.Signal.Effects (chainEffects, gain)
import Sonora.Synth.Instrument (Instrument (..))
import Sonora.Synth.Patch (Patch, compilePatch)
import Sonora.Synth.Synthesizer (masterLimiter)
import Sonora.Synth.Voice (renderVoice)

data AuditionNote = AuditionNote
    { notePitch    :: !Int     -- ^ MIDI note number.
    , noteVelocity :: !Double  -- ^ 0 to 1.
    , noteGate     :: !Double  -- ^ Seconds the key is held.
    } deriving (Eq, Show)

-- | Level of an audition before the limiter. Not normalised, so every
-- level control is heard as it would change the track.
auditionGain :: Double
auditionGain = 0.5

-- | How long a held key sounds before it releases on its own.
holdSeconds :: Double
holdSeconds = 3

minPitch, maxPitch :: Int
minPitch = 21
maxPitch = 108

renderAudition :: Int -> Patch -> AuditionNote -> V.Vector Frame
renderAudition sampleRate patch n =
    let inst = compilePatch patch
        cfg = defaultConfig { configSampleRate = sampleRate }
        voice = renderVoice (instVoice inst) (midiToFreq (notePitch n)) (max 0 (noteGate n)) (noteVelocity n)
        sig = masterLimiter (gain auditionGain (chainEffects (instEffects inst) voice))
    in trimSilence (runSignal sig cfg)

-- | Drop the inaudible end of effect tails (below -80 dB).
trimSilence :: V.Vector Frame -> V.Vector Frame
trimSilence v =
    let quiet (Frame l r) = abs l < 1e-4 && abs r < 1e-4
        end = go (V.length v)
        go k | k > 0 && quiet (V.unsafeIndex v (k - 1)) = go (k - 1)
             | otherwise = k
    in V.take (max 1 end) v
