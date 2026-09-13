{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonora.Render.Engine
    ( Track (..)
    , Song (..)
    , defaultSong
    , renderTrack
    , renderSong
    , normalizeSignal
    , renderSongToWAV
    , renderMusicToWAV
    ) where

import qualified Data.Vector.Storable as V
import Sonora.Music.Composition (Music)
import Sonora.Music.Duration (BPM (..), TimeSignature (..), defaultBPM, defaultTimeSignature)
import Sonora.Music.Note (Note)
import Sonora.Render.WAV (renderSignalToWAV)
import Sonora.Signal.Core
import Sonora.Synth.Instrument (Instrument)
import Sonora.Synth.Synthesizer (masterLimiter, renderComposition)

-- | A named track associating a musical score with an instrument.
data Track = Track
    { trackName       :: !String
    , trackInstrument :: !Instrument
    , trackMusic      :: !(Music Note)
    }

-- | A multi-track musical composition project.
data Song = Song
    { songTitle   :: !String
    , songBPM     :: !BPM
    , songTimeSig :: !TimeSignature
    , songTracks  :: ![Track]
    }

defaultSong :: Song
defaultSong = Song
    { songTitle   = "Untitled"
    , songBPM     = defaultBPM
    , songTimeSig = defaultTimeSignature
    , songTracks  = []
    }

-- | Render a single track to an audio Signal.
renderTrack :: BPM -> Track -> Signal
renderTrack bpm Track{..} =
    renderComposition bpm trackInstrument trackMusic

-- | Render an entire multi-track Song to an audio Signal.
-- Mixes all tracks in parallel and passes through master limiting.
renderSong :: Song -> Signal
renderSong Song{..} =
    let trackSignals = map (renderTrack songBPM) songTracks
        mixed        = foldl mixSignals emptySignal trackSignals
    in masterLimiter mixed

-- | Normalize a signal so its absolute peak matches targetPeak (e.g. 0.95).
normalizeSignal :: Double -> Signal -> Signal
normalizeSignal targetPeak sig = Signal $ \cfg ->
    let inVec = runSignal sig cfg
        findMaxPeak !curMax !i
            | i >= V.length inVec = curMax
            | otherwise =
                let Frame l r = inVec V.! i
                    !m = max curMax (max (abs l) (abs r))
                in findMaxPeak m (i + 1)
        peak = findMaxPeak 0.0 0
    in if peak <= 1e-6
       then inVec
       else let !gainFactor = targetPeak / peak
            in V.map (scaleFrame gainFactor) inVec

-- | Render a multi-track Song and export directly to a 16-bit PCM WAV file.
renderSongToWAV :: FilePath -> Song -> IO ()
renderSongToWAV path song =
    let sig = normalizeSignal 0.95 (renderSong song)
    in renderSignalToWAV path sig

-- | Render an individual Music score to a WAV file.
renderMusicToWAV :: FilePath -> BPM -> Instrument -> Music Note -> IO ()
renderMusicToWAV path bpm inst music =
    let sig = normalizeSignal 0.95 (renderComposition bpm inst music)
    in renderSignalToWAV path sig
