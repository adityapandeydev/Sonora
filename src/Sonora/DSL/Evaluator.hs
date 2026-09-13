{-# LANGUAGE RecordWildCards #-}
module Sonora.DSL.Evaluator
    ( evaluateSong
    , evaluateTrack
    , evaluateItem
    ) where

import qualified Data.Map.Strict as Map
import Sonora.DSL.AST
import Sonora.Music.Chord (Chord (..))
import Sonora.Music.Composition
import Sonora.Music.Duration (defaultBPM, defaultTimeSignature)
import Sonora.Music.Note (Articulation (..), Note (..), mf)
import Sonora.Render.Engine (Song (..), Track (..))
import Sonora.Signal.Envelope (adsr)
import Sonora.Signal.Filter (FilterType (..))
import Sonora.Signal.Oscillator (Waveform (..))
import Sonora.Synth.Instrument
import Sonora.Synth.Voice

-- | Evaluate an ASTItem into a typed Music Note structure.
evaluateItem :: ASTItem -> Music Note
evaluateItem (ASTItemNote (ASTNote pc dur mVel mArt)) =
    let vel = maybe mf id mVel
        art = maybe Normal id mArt
    in Prim (Note pc dur vel art)

evaluateItem (ASTItemRest dur) =
    Rest dur

evaluateItem (ASTItemChord pitches dur mVel) =
    let vel = maybe mf id mVel
    in chordGroup [ Prim (Note pc dur vel Normal) | pc <- pitches ]

evaluateItem (ASTItemNamedChord root quality dur) =
    chordToMusic (Chord root quality 0) dur

evaluateItem (ASTItemRepeat n items) =
    repeatM n (line (map evaluateItem items))

-- | Construct an Instrument from an AST instrument definition.
evaluateInstrumentDef :: ASTInstrumentDef -> Instrument
evaluateInstrumentDef ASTInstrumentDef{..} =
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
    in Instrument astInstName voice []

-- | Evaluate an AST track into a typed Track given an instrument environment.
evaluateTrack :: Map.Map String Instrument -> ASTTrack -> Track
evaluateTrack instMap ASTTrack{..} =
    let inst = case Map.lookup astTrackInstrument instMap of
            Just i  -> i
            Nothing -> maybe defaultInstrument id (lookupInstrument astTrackInstrument)
        music = line (map evaluateItem astTrackItems)
    in Track astTrackName inst music

-- | Evaluate an entire parsed ASTSong into a fully-typed Sonora Song.
evaluateSong :: ASTSong -> Song
evaluateSong ASTSong{..} =
    let customInsts = Map.fromList [ (astInstName def, evaluateInstrumentDef def) | def <- astSongInstruments ]
        instEnv     = customInsts `Map.union` defaultInstruments
        tracks      = map (evaluateTrack instEnv) astSongTracks
        bpm         = maybe defaultBPM id astSongBPM
        timeSig     = maybe defaultTimeSignature id astSongTimeSig
    in Song astSongTitle bpm timeSig tracks
