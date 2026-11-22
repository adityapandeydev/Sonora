-- | Rendering a compiled project to an audio buffer, with progress.
module Sonora.Project.Render
    ( renderCompiled
    ) where

import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Vector.Storable as V

import Sonora.Project.Compile
import Sonora.Render.Engine (normalizeSignal, renderTrack)
import Sonora.Signal.Core
import Sonora.Synth.Synthesizer (masterLimiter)

-- | Render the song at the given sample rate. Produces the same samples as
-- @runSignal (normalizeSignal 0.95 (renderSong song))@, computed track by
-- track so progress can be reported as @(completed, total)@ steps (one per
-- track plus a final mixdown). Evaluation happens in this thread, so
-- killing the thread stops the work.
renderCompiled :: Int -> (Int -> Int -> IO ()) -> CompiledProject -> IO (V.Vector Frame)
renderCompiled sampleRate progress c = do
    let cfg = defaultConfig { configSampleRate = sampleRate }
        tracks = map compiledTrack (compiledTracks c)
        total = length tracks + 1
    progress 0 total
    rendered <- forM (zip [1 ..] tracks) $ \(k, t) -> do
        v <- evaluate (runSignal (renderTrack (compiledBPM c) t) cfg)
        progress k total
        return v
    let mixed = masterLimiter (foldl mixSignals emptySignal [ Signal (const v) | v <- rendered ])
    out <- evaluate (runSignal (normalizeSignal 0.95 mixed) cfg)
    progress total total
    return out
