{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sonora.Generate.Random
    ( Seed (..)
    , PRNG
    , mkPRNG
    , nextWord64
    , nextDouble
    , nextIntRange
    , nextDoubleRange
    , chooseRandom
    , shuffle
    ) where

import Data.Bits (shiftR, xor)
import Data.Word (Word64)

-- | A random seed value for deterministic generation.
newtype Seed = Seed { getSeed :: Word64 }
    deriving (Eq, Ord, Num, Show, Read)

-- | Pure SplitMix64 pseudo-random generator state.
newtype PRNG = PRNG Word64
    deriving (Eq, Show)

-- | Initialize a PRNG from a seed.
mkPRNG :: Seed -> PRNG
mkPRNG (Seed s) = PRNG (if s == 0 then 0x853c49e6748fea9b else s)

-- | Generate next pseudo-random 64-bit word and advanced generator state.
nextWord64 :: PRNG -> (Word64, PRNG)
nextWord64 (PRNG state) =
    let !s = state + 0x9e3779b97f4a7c15
        !z0 = s
        !z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
        !result = z2 `xor` (z2 `shiftR` 31)
    in (result, PRNG s)

-- | Generate a random double in [0.0, 1.0).
nextDouble :: PRNG -> (Double, PRNG)
nextDouble gen =
    let (w, gen') = nextWord64 gen
        d = fromIntegral (w `shiftR` 11) * (1.0 / 9007199254740992.0)
    in (d, gen')

-- | Generate an integer in the inclusive range [low, high].
nextIntRange :: (Int, Int) -> PRNG -> (Int, PRNG)
nextIntRange (low, high) gen
    | low >= high = (low, gen)
    | otherwise =
        let (d, gen') = nextDouble gen
            range = fromIntegral (high - low + 1)
            val   = low + floor (d * range)
        in (val, gen')

-- | Generate a double in [low, high].
nextDoubleRange :: (Double, Double) -> PRNG -> (Double, PRNG)
nextDoubleRange (low, high) gen =
    let (d, gen') = nextDouble gen
    in (low + d * (high - low), gen')

-- | Pick a random element from a non-empty list.
chooseRandom :: [a] -> PRNG -> (a, PRNG)
chooseRandom [] _ = error "chooseRandom: empty list"
chooseRandom xs gen =
    let (idx, gen') = nextIntRange (0, length xs - 1) gen
    in (xs !! idx, gen')

-- | Deterministically shuffle a list (Fisher-Yates shuffle).
shuffle :: [a] -> PRNG -> ([a], PRNG)
shuffle [] gen = ([], gen)
shuffle [x] gen = ([x], gen)
shuffle xs gen = go (length xs) xs gen
  where
    go 0 acc g = (acc, g)
    go i acc g =
        let (j, g') = nextIntRange (0, i - 1) g
            (item, rest) = extractAt j acc
            (shuffledRest, g'') = go (i - 1) rest g'
        in (item : shuffledRest, g'')

    extractAt 0 (y:ys) = (y, ys)
    extractAt k (y:ys) =
        let (found, remaining) = extractAt (k - 1) ys
        in (found, y : remaining)
    extractAt _ [] = error "extractAt: index out of bounds"
