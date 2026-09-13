module DeterminismSpec (determinismTests) where

import Test.QuickCheck
import Data.Word (Word64)
import qualified Data.Vector.Storable as V
import Sonora.Generate.Procedural
import Sonora.Generate.Random
import Sonora.Music.Duration (BPM (..))
import Sonora.Music.Pitch (c4)
import Sonora.Music.Scale (ScaleType (..))
import Sonora.Render.Engine
import Sonora.Signal.Core

-- | Property: Identical seed produces identical composition structure.
prop_seedGeneratesIdenticalSong :: Word64 -> Bool
prop_seedGeneratesIdenticalSong s =
    let song1 = generateProceduralSong (Seed s) Major c4 (BPM 120) 2
        song2 = generateProceduralSong (Seed s) Major c4 (BPM 120) 2
    in songTitle song1 == songTitle song2
       && length (songTracks song1) == length (songTracks song2)

-- | Property: Rendering with the same seed produces exact, bit-identical audio samples.
prop_seedProducesBitExactAudio :: Word64 -> Bool
prop_seedProducesBitExactAudio s =
    let song1 = generateProceduralSong (Seed s) Major c4 (BPM 120) 1
        song2 = generateProceduralSong (Seed s) Major c4 (BPM 120) 1
        cfg = AudioConfig 22050 2  -- Quick sample rate for fast property checks
        v1 = runSignal (renderSong song1) cfg
        v2 = runSignal (renderSong song2) cfg
    in v1 == v2

determinismTests :: IO Bool
determinismTests = do
    putStrLn "--- Running Procedural Music Determinism Tests ---"
    r1 <- quickCheckResult prop_seedGeneratesIdenticalSong
    r2 <- quickCheckResult prop_seedProducesBitExactAudio
    let allPassed = all isSuccess [r1, r2]
    return allPassed
