-- | Top-level module for Sonora: Functional Music Synthesis & Composition Engine.
module Sonora
    ( -- * Music Theory & Primitives
      module Sonora.Music.Pitch
    , module Sonora.Music.Interval
    , module Sonora.Music.Duration
    , module Sonora.Music.Note
    , module Sonora.Music.Scale
    , module Sonora.Music.Chord
    , module Sonora.Music.Composition

      -- * DSP & Signal Processing
    , module Sonora.Signal.Core
    , module Sonora.Signal.Oscillator
    , module Sonora.Signal.Envelope
    , module Sonora.Signal.Filter
    , module Sonora.Signal.Effects

      -- * Synthesizer & Instruments
    , module Sonora.Synth.Voice
    , module Sonora.Synth.Instrument
    , module Sonora.Synth.Synthesizer

      -- * Rendering & WAV Output
    , module Sonora.Render.WAV
    , module Sonora.Render.Engine

      -- * Sonora Score DSL
    , module Sonora.DSL.AST
    , module Sonora.DSL.Parser
    , module Sonora.DSL.Evaluator

      -- * Algorithmic & Procedural Generation
    , module Sonora.Generate.Random
    , module Sonora.Generate.Melody
    , module Sonora.Generate.Progression
    , module Sonora.Generate.Procedural
    ) where

import Sonora.Music.Pitch hiding (f)
import Sonora.Music.Interval
import Sonora.Music.Duration
import Sonora.Music.Note
import Sonora.Music.Scale
import Sonora.Music.Chord
import Sonora.Music.Composition

import Sonora.Signal.Core
import Sonora.Signal.Oscillator
import Sonora.Signal.Envelope
import Sonora.Signal.Filter
import Sonora.Signal.Effects

import Sonora.Synth.Voice
import Sonora.Synth.Instrument
import Sonora.Synth.Synthesizer

import Sonora.Render.WAV
import Sonora.Render.Engine

import Sonora.DSL.AST
import Sonora.DSL.Parser
import Sonora.DSL.Evaluator

import Sonora.Generate.Random
import Sonora.Generate.Melody
import Sonora.Generate.Progression
import Sonora.Generate.Procedural
