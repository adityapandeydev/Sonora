-- | Rendering a compiled project: each track to a stem, then the mix.
--
-- Stems are cached by what decides their sound: sample rate, tempo, the
-- track's notes and its patch. Changing the mix only remixes; changing one
-- track's notes or sound renders only that track again.
module Sonora.Project.Render
    ( Stems
    , emptyStems
    , stemCount
    , renderProject
    , renderProjectWith
    , renderStem
    , renderCompiled
    ) where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as V

import Sonora.DSL.AST (ASTItem)
import Sonora.Music.Duration (BPM (..))
import Sonora.Project.Compile
import Sonora.Project.Mix (mixdown, songGain)
import Sonora.Project.Types (TrackId)
import Sonora.Render.Engine (renderTrack)
import Sonora.Signal.Core
import Sonora.Synth.Patch (Patch)

data StemKey = StemKey !Int !Double !Patch ![ASTItem]
    deriving Eq

data Stems = Stems
    { stemTracks :: !(Map.Map TrackId (StemKey, V.Vector Frame))
    , stemGain   :: !(Maybe ([StemKey], Double))  -- ^ Song gain for a set of stems.
    }

emptyStems :: Stems
emptyStems = Stems Map.empty Nothing

stemCount :: Stems -> Int
stemCount = Map.size . stemTracks

keyOf :: Int -> CompiledProject -> CompiledTrack -> StemKey
keyOf sr c t = StemKey sr (getBPM (compiledBPM c)) (compiledPatch t) (compiledItems t)

-- | Synthesize one track at a sample rate.
renderStem :: Int -> CompiledProject -> CompiledTrack -> IO (V.Vector Frame)
renderStem sr c t = evaluate (runSignal (renderTrack (compiledBPM c) (compiledTrack t)) defaultConfig { configSampleRate = sr })

renderProject :: Int -> (Int -> Int -> IO ()) -> Stems -> CompiledProject -> IO (Stems, V.Vector Frame)
renderProject = renderProjectWith renderStem

-- | Render with a given stem synthesizer, reusing stems that still apply.
-- Progress is @(completed, total)@: one step per stem to synthesize, plus the
-- mix. Evaluation happens in this thread, so killing it stops the work.
-- Stems of tracks no longer in the project are dropped.
renderProjectWith :: (Int -> CompiledProject -> CompiledTrack -> IO (V.Vector Frame))
                  -> Int -> (Int -> Int -> IO ()) -> Stems -> CompiledProject -> IO (Stems, V.Vector Frame)
renderProjectWith synth sr progress cache c = do
    let tracks = compiledTracks c
        keys = map (keyOf sr c) tracks
        reusable t k = case Map.lookup (compiledTrackId t) (stemTracks cache) of
            Just (k', v) | k' == k -> Just v
            _ -> Nothing
        todo = length [ () | (t, k) <- zip tracks keys, Nothing <- [reusable t k] ]
        total = todo + 1
    progress 0 total
    let go _ [] = return []
        go done ((t, k) : rest) = case reusable t k of
            Just v -> ((t, k, v) :) <$> go done rest
            Nothing -> do
                v <- synth sr c t
                progress (done + 1) total
                ((t, k, v) :) <$> go (done + 1) rest
    stems <- go 0 (zip tracks keys)
    let gain = case stemGain cache of
            Just (ks, g) | ks == keys -> g
            _ -> songGain [ v | (_, _, v) <- stems ]
    out <- evaluate (mixdown gain (compiledMaster c) [ (compiledMix t, v) | (t, _, v) <- stems ])
    progress total total
    return (Stems (Map.fromList [ (compiledTrackId t, (k, v)) | (t, k, v) <- stems ]) (Just (keys, gain)), out)

-- | Render without a cache.
renderCompiled :: Int -> (Int -> Int -> IO ()) -> CompiledProject -> IO (V.Vector Frame)
renderCompiled sr progress c = snd <$> renderProject sr progress emptyStems c
