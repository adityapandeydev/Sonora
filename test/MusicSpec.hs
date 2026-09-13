{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module MusicSpec (musicTests) where

import Test.QuickCheck
import Sonora.Music.Composition
import Sonora.Music.Duration
import Sonora.Music.Interval
import Sonora.Music.Note
import Sonora.Music.Pitch

-- QuickCheck Arbitrary instances
instance Arbitrary Pitch where
    arbitrary = elements [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

instance Arbitrary Octave where
    arbitrary = Octave <$> choose (0, 8)

instance Arbitrary PitchClass where
    arbitrary = PitchClass <$> arbitrary <*> arbitrary

instance Arbitrary Duration where
    arbitrary = elements [whole, half, quarter, eighth, sixteenth]

instance Arbitrary Velocity where
    arbitrary = Velocity <$> choose (0.1, 1.0)

instance Arbitrary Articulation where
    arbitrary = elements [Normal, Staccato, Legato, Tenuto]

instance Arbitrary Note where
    arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Small arbitrary trees for Music
instance Arbitrary a => Arbitrary (Music a) where
    arbitrary = sized musicTree
      where
        musicTree 0 = Prim <$> arbitrary
        musicTree n = oneof
            [ Prim <$> arbitrary
            , Rest <$> arbitrary
            , do
                let n2 = n `div` 2
                a <- musicTree n2
                b <- musicTree n2
                elements [Sequential a b, Parallel a b]
            ]

-- Properties

-- | Property 1: MIDI pitch conversion is a true bijection / roundtrip.
prop_midiRoundtrip :: PitchClass -> Bool
prop_midiRoundtrip pc =
    let midi = pitchToMidi pc
    in midi < 0 || midi > 127 || midiToPitch midi == pc

-- | Property 2: Octave interval transposition shifts octave by exactly 1.
prop_octaveTransposition :: PitchClass -> Bool
prop_octaveTransposition pc@(PitchClass p (Octave o)) =
    o >= 7 || transposePitch (Interval 12) pc == PitchClass p (Octave (o + 1))

-- | Property 3: Duration of sequential composition is the sum of durations.
prop_seqDuration :: Music Note -> Music Note -> Bool
prop_seqDuration a b =
    musicDuration noteDuration (a <> b) == musicDuration noteDuration a + musicDuration noteDuration b

-- | Property 4: Duration of parallel composition is the maximum of durations.
prop_parDuration :: Music Note -> Music Note -> Bool
prop_parDuration a b =
    musicDuration noteDuration (a ||| b) == max (musicDuration noteDuration a) (musicDuration noteDuration b)

-- | Property 5: Monoid left and right identity laws for duration.
prop_monoidIdentity :: Music Note -> Bool
prop_monoidIdentity m =
    musicDuration noteDuration (mempty <> m) == musicDuration noteDuration m
    && musicDuration noteDuration (m <> mempty) == musicDuration noteDuration m

-- Test Runner
musicTests :: IO Bool
musicTests = do
    putStrLn "--- Running Music Theory & Algebra Property Tests ---"
    r1 <- quickCheckResult prop_midiRoundtrip
    r2 <- quickCheckResult prop_octaveTransposition
    r3 <- quickCheckResult prop_seqDuration
    r4 <- quickCheckResult prop_parDuration
    r5 <- quickCheckResult prop_monoidIdentity
    let allPassed = all isSuccess [r1, r2, r3, r4, r5]
    return allPassed
