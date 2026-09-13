{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonora.Generate.Procedural
    ( generateProceduralSong
    , songToDSL
    ) where

import Data.List (intercalate)
import Sonora.Generate.Melody
import Sonora.Generate.Progression
import Sonora.Generate.Random
import Sonora.Music.Chord (Chord (..))
import Sonora.Music.Composition
import Sonora.Music.Duration
import Sonora.Music.Interval (Interval (..), transposePitch)
import Sonora.Music.Note
import Sonora.Music.Pitch (PitchClass)
import Sonora.Music.Scale
import Sonora.Render.Engine (Song (..), Track (..))
import Sonora.Synth.Instrument

-- | Generate a complete multi-track song deterministically from a numeric seed.
generateProceduralSong :: Seed -> ScaleType -> PitchClass -> BPM -> Int -> Song
generateProceduralSong (Seed sVal) sType root bpm numBars =
    let scl = Scale root sType
        -- Seed derivations for independent track variation
        seedChords = Seed (sVal * 6364136223846793005 + 1)
        seedMelody = Seed (sVal * 1442695040888963407 + 1)

        -- Harmonic progression: 1 chord per bar (whole note duration)
        rawChords = generateProgression seedChords scl numBars
        chords = voiceLead rawChords
        padMusic = progressionToMusic chords whole
        padTrack = Track "Chords" padSynth padMusic

        -- Lead Melody: 4-8 notes per bar
        durPalette = [quarter, eighth, dotted quarter]
        melodyMusic = generateMelody seedMelody scl 8 durPalette (numBars * 4)
        leadTrack   = Track "Melody" leadSynth melodyMusic

        -- Bassline: rhythmic driving eighth notes on the root pitch transposed down 2 octaves
        genBarBass ch =
            let bassRoot = transposePitch (Interval (-24)) (chordRoot ch)
                bassNote = Note bassRoot eighth mf Normal
            in repeatM 8 (singleNote bassNote)
        bassMusic = line (map genBarBass chords)
        bassTrack = Track "Bassline" bassSynth bassMusic

        title = "Procedural Session (Seed " ++ show sVal ++ ")"
    in Song title bpm defaultTimeSignature [padTrack, leadTrack, bassTrack]

-- | Format a Sonora Song as clean, human-readable declarative .sonora DSL text.
songToDSL :: Song -> String
songToDSL Song{..} =
    let header = unlines
            [ "# Sonora Declarative Music Score"
            , "# Title: " ++ songTitle
            , "tempo " ++ show (getBPM songBPM)
            , "time 4/4"
            , ""
            ]
        formatTrack (Track name inst music) =
            let events = linearize noteDuration music
                trackHeader = "track " ++ filter (/= ' ') name ++ " (" ++ instName inst ++ "):"
                formatEvent (TimedEvent _ dur _ Note{..}) =
                    "    " ++ show notePitch ++ " " ++ show dur ++ " " ++ show noteVelocity
                eventLines = map formatEvent events
            in unlines (trackHeader : eventLines)
        body = intercalate "\n" (map formatTrack songTracks)
    in header ++ body
