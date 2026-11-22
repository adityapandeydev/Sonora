{-# LANGUAGE BangPatterns #-}
-- | Waveform overviews of audio buffers.
module Sonora.Signal.Waveform
    ( waveformPeaks
    ) where

import qualified Data.Vector.Storable as V

import Sonora.Signal.Core (Frame (..))

-- | Split a buffer into at most the given number of equal spans and return
-- the lowest and highest sample of each, over both channels. Every frame
-- belongs to exactly one span; a buffer shorter than the requested count
-- yields one span per frame.
waveformPeaks :: Int -> V.Vector Frame -> [(Double, Double)]
waveformPeaks buckets frames
    | n <= 0 = []
    | otherwise = [ peak (bound i) (bound (i + 1)) | i <- [0 .. n - 1] ]
  where
    len = V.length frames
    n = min buckets len
    bound i = i * len `quot` n
    peak from to = V.foldl' widen (first, first') (V.slice from (to - from) frames)
      where
        Frame l r = V.unsafeIndex frames from
        !first = min l r
        !first' = max l r
    widen (!lo, !hi) (Frame l r) =
        let !lo' = min lo (min l r)
            !hi' = max hi (max l r)
        in (lo', hi')
