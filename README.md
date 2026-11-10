# Sonora 🎵

> **A functional music synthesis, digital signal processing (DSP), and composition engine written in pure Haskell.**

[![Haskell](https://img.shields.io/badge/Language-Haskell2010-purple.svg)](https://www.haskell.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.4%20|%209.6%20|%209.10-orange.svg)](https://www.haskell.org/ghc/)

Sonora demonstrates that music, audio signals, subtractive synthesizers, DSP effects, and algorithmic compositions can be expressed as **pure, composable, strongly typed functional values** in idiomatic Haskell.

---

## Core Philosophy

> **The audio engine is the product. The frontend is an afterthought.**

Sonora is not a shallow Haskell binding around an external C library. The core synthesis, filter algorithms, envelope generators, Schroeder reverb, parser combinators, and RIFF WAV encoding are implemented natively in Haskell, strictly following a decoupled pipeline:

```text
Composition
     ↓
Music Representation
     ↓
Synthesis
     ↓
DSP
     ↓
Audio Rendering
     ↓
WAV / Audio Output
```

---

## Architectural Highlights

* **Strong Domain Modeling**: Expressive types for `Pitch`, `Interval`, `Scale`, `Chord`, `Velocity`, and exact rational `Duration` without floating-point accumulation drift.
* **Music as an Algebra**: Composable `Music a` data structure featuring `Semigroup` (`<>` or `|:>`) for sequential chaining and `(|||)` for parallel polyphony.
* **Pure DSP & Zero Space Leaks**: Chunked, unboxed storable frame vectors avoid lazy evaluation thunk accumulation while maintaining pure functional composition.
* **Cytomic State-Variable Filter (SVF)**: Andrew Simper's trapezoidal integrated zero-delay feedback filter providing simultaneous Low-Pass, High-Pass, Band-Pass, and Notch filtering with absolute numerical stability.
* **Algorithmic Reverb**: A pure functional Schroeder topology comprising 4 parallel feedback comb filters and 2 series allpass filters.
* **Declarative DSL**: A human-readable `.sonora` music score language parsed with parser combinators.
* **Deterministic Procedural Music**: Seed-based algorithmic generation of melodies, chord progressions with smooth voice leading, and driving basslines.
* **Pure Haskell WAV Serialization**: Canonical RIFF/WAV file generation (16/24-bit PCM & 32-bit float) with zero native dependencies.
* **Haskell Audio Playback**: Mixing, transport (play, pause, sample-accurate seek and looping), smoothed gain, and separate song and audition voices are implemented in Haskell. On Windows, finished sample blocks go to the system audio engine through a direct FFI binding to WASAPI, with no third-party native code.

---

## Building and Testing

### Prerequisites
* GHC 9.4+ (or GHC 9.10+)
* Cabal 3.8+
* Windows 10 or later (x86_64) for audio playback; rendering and the rest of the library work on any GHC platform

```bash
# Update cabal package database
cabal update

# Build the library and CLI
cabal build

# Run property-based and DSP tests
cabal test
```

---

## CLI Usage

Sonora provides a comprehensive CLI for rendering scores, synthesizing notes, and generating procedural music:

### 1. Render a Score to WAV
```bash
# Render a .sonora score to CD-quality 16-bit 44.1kHz WAV
cabal run sonora -- render examples/bach_prelude.sonora -o bach.wav

# Render a multi-track synthwave track in studio 24-bit 48kHz
cabal run sonora -- render examples/synthwave.sonora -o synthwave.wav --sample-rate 48000 --bit-depth 24
```

### 2. Inspect a Score
Analyze metadata, time signature, track list, notes, and duration:
```bash
cabal run sonora -- inspect examples/synthwave.sonora
```

### 3. Procedural Algorithmic Music Generation
Generate completely deterministic music from a seed:
```bash
# Generate score file from seed 42 in D-Dorian
cabal run sonora -- generate --seed 42 --scale D-Dorian --bars 8 -o doriandream.sonora

# Directly render to WAV
cabal run sonora -- generate --seed 1337 --scale A-Minor --bars 16 --render ambient.wav
```

### 4. Fast One-Liner Synth
Synthesize notes directly from the command line:
```bash
cabal run sonora -- synth --preset lead -n "C4 quarter E4 quarter G4 half" -o test.wav
```

### 5. Playback
Render a score and play it on the default audio output device:
```bash
cabal run sonora -- play examples/synthwave.sonora

# Stop after a fixed time (useful for quick device checks)
cabal run sonora -- play examples/synthwave.sonora --max-seconds 3
```
The command exits with a non-zero status and a message if no output device is available or the device disappears during playback.

---

## Haskell Library API Guide

### 1. Composable Music Algebra

```haskell
import Sonora

-- Define a sequential melody
melody :: Music Note
melody =
    note c4 quarter
    |:> note e4 quarter
    |:> note g4 half

-- Define a parallel bassline
bass :: Music Note
bass =
    note c2 whole
    ||| note g2 whole

-- Compose in parallel
song :: Music Note
song = melody ||| bass
```

### 2. Scales and Chords

```haskell
-- Scales as pure mathematical transformations
cMajor = scaleNotes (majorScale c4)
-- => [C4, D4, E4, F4, G4, A4, B4]

-- Chords and inversions
cMaj7 = chordNotes (maj7Chord c4)
-- => [C4, E4, G4, B4]

cMaj7Invert1 = chordNotes (invertChord 1 (maj7Chord c4))
-- => [E4, G4, B4, C5]
```

### 3. DSP & Audio Effects Pipeline

```haskell
-- Custom signal processing chain
customLead :: Signal -> Signal
customLead =
    chainEffects
        [ chorus 1.5 0.3 0.25         -- Modulated stereo chorus
        , delay 0.25 0.35 0.30         -- Stereo delay with feedback
        , schroederReverb 0.7 0.35     -- 4-comb, 2-allpass Schroeder reverb
        ]

-- Render directly to WAV
main :: IO ()
main = do
    let sig = renderComposition (BPM 120) leadSynth song
    writeWAVFile "output.wav" BitDepth16 defaultConfig (masterLimiter sig)
```

---

## Declarative Score DSL (`.sonora`)

Sonora includes a clean, declarative language for composing music without writing Haskell code:

```text
title "Synthwave Outrun"
tempo 124
time 4/4

track bass (bass):
    A1 eighth
    A1 eighth
    F1 eighth
    G1 eighth

track pad (pad):
    chord(A3, C4, E4) whole
    chord(F3, A3, C4) whole

track lead (lead):
    E4 quarter
    G4 quarter
    A4 half
```

---

## Project Structure

```text
sonora/
├── app/
│   └── Main.hs                  # CLI application entrypoint
├── src/
│   ├── Sonora.hs                # Master engine re-exports
│   ├── Sonora/Music/            # Domain model, interval theory, algebra
│   ├── Sonora/Signal/           # Oscillators, envelopes, SVF filter, effects
│   ├── Sonora/Synth/            # Subtractive voices and instrument presets
│   ├── Sonora/Render/           # RIFF WAV encoder and multi-track engine
│   ├── Sonora/Audio/            # Realtime mixer, transport model, WASAPI driver, test fake
│   ├── Sonora/DSL/              # Lexer, Parsec parser, AST, and evaluator
│   └── Sonora/Generate/         # Deterministic PRNG and algorithmic generators
├── test/                        # QuickCheck property and unit test suites
├── examples/                    # Pre-composed .sonora scores
└── sonora.cabal                 # Package specification
```

---

## Verification & Property-Based Tests

Run the test suite:
```bash
cabal test --test-show-details=always
```

Verified properties include:
- **MIDI Bijective Roundtrip**: `midiToPitch (pitchToMidi p) == p`.
- **Sequential Duration Invariant**: `duration (a <> b) == duration a + duration b`.
- **Parallel Duration Invariant**: `duration (a ||| b) == max (duration a) (duration b)`.
- **Monoid Laws**: Identity and associativity for composition and signals.
- **DSP Bounded Amplitudes**: Numerical range limits and Cytomic SVF stability.
- **Deterministic Reproducibility**: Bit-identical audio vectors from identical seeds.
- **Transport Model**: The realtime mixer's output matches the reference transport model sample for sample (seek, loop, end of buffer) for arbitrary device period sizes.
