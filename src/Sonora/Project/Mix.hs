{-# LANGUAGE BangPatterns #-}
-- | Mixing rendered tracks (stems) into the song, for playback and export.
--
-- > out = limit (masterGain * songGain * sum [ pan p (gain g stem) | audible ])
--
-- * Audible tracks: when any track is soloed, the soloed tracks that are not
--   muted; otherwise every track that is not muted.
-- * Pan is equal-power, exactly unity at the centre.
-- * 'songGain' is fixed by the stems alone: it brings the unity mix of all
--   tracks to a 0.95 peak. Mix settings never change it, so every gain,
--   mute and solo is heard as set; nothing renormalises them away.
-- * The limiter leaves everything up to 0.95 untouched and softly bounds
--   anything louder, never past full scale, so raising gains cannot clip.
module Sonora.Project.Mix
    ( audible
    , panGains
    , songGain
    , mixdown
    , limitSample
    , targetPeak
    ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import Sonora.Project.Types (MasterMix (..), TrackMix (..))
import Sonora.Signal.Core (Frame (..))

targetPeak :: Double
targetPeak = 0.95

audible :: [TrackMix] -> [Bool]
audible mixes =
    let soloing = any mixSolo mixes
    in [ not (mixMute m) && (not soloing || mixSolo m) | m <- mixes ]

-- | Left and right gains for a track gain and a pan from -1 to 1.
panGains :: Double -> Double -> (Double, Double)
panGains g p
    | p == 0 = (g, g)
    | otherwise =
        let angle = (max (-1) (min 1 p) + 1) * (pi / 4)
        in (g * cos angle * sqrt 2, g * sin angle * sqrt 2)

-- | The gain that brings the unity mix of all stems to 'targetPeak'.
songGain :: [V.Vector Frame] -> Double
songGain stems =
    let peak = peakOf (accumulate [ ((1, 1), s) | s <- stems ])
    in if peak <= 1e-6 then 1 else targetPeak / peak

mixdown :: Double -> MasterMix -> [(TrackMix, V.Vector Frame)] -> V.Vector Frame
mixdown gain (MasterMix master) tracks =
    let heard = [ (panGains (mixGain m) (mixPan m), s) | ((m, s), True) <- zip tracks (audible (map fst tracks)) ]
        total = master * gain
        len = maximum (0 : map (V.length . snd) tracks)
        summed = accumulate heard
        padded = if V.length summed < len then summed V.++ V.replicate (len - V.length summed) (Frame 0 0) else summed
    in V.map (\(Frame l r) -> Frame (limitSample (total * l)) (limitSample (total * r))) padded

-- | Sum stems with per-channel gains, in order, starting from silence.
accumulate :: [((Double, Double), V.Vector Frame)] -> V.Vector Frame
accumulate xs = V.create $ do
    let len = maximum (0 : map (V.length . snd) xs)
    out <- MV.replicate len (Frame 0 0)
    forM_ xs $ \((gl, gr), v) -> addInto out gl gr v
    return out

addInto :: MV.MVector s Frame -> Double -> Double -> V.Vector Frame -> ST s ()
addInto out gl gr v = go 0
  where
    n = V.length v
    unity = gl == 1 && gr == 1
    go !i = when (i < n) $ do
        Frame a b <- MV.unsafeRead out i
        let Frame l r = V.unsafeIndex v i
        MV.unsafeWrite out i (if unity then Frame (a + l) (b + r) else Frame (a + gl * l) (b + gr * r))
        go (i + 1)

peakOf :: V.Vector Frame -> Double
peakOf = V.foldl' (\m (Frame l r) -> max m (max (abs l) (abs r))) 0

-- | Unchanged up to 'targetPeak'; above it, a soft curve towards full scale.
limitSample :: Double -> Double
limitSample x
    | abs x <= targetPeak = x
    | x > 0 = targetPeak + (1 - targetPeak) * tanh ((x - targetPeak) / (1 - targetPeak))
    | otherwise = negate (limitSample (negate x))
