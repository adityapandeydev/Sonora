{-# LANGUAGE OverloadedStrings #-}
module HistorySpec (historyTests) where

import Control.Exception (SomeException, catch, displayException)
import Data.Maybe (fromJust)
import Data.Text (Text)

import Sonora.Project.History
import Sonora.Project.Types
import Sonora.Project.Update
import Sonora.Synth.Patch (lookupPresetPatch)

check :: String -> IO Bool -> IO Bool
check name t = do
    passed <- t `catch` \e -> do
        putStrLn ("    exception: " ++ displayException (e :: SomeException))
        return False
    putStrLn $ "  " ++ name ++ ": " ++ (if passed then "PASSED" else "FAILED")
    return passed

score :: Text
score = "track lead (lead):\n    C4 quarter\ntrack bass (bass):\n    C2 half\n"

idOf :: Text -> Project -> TrackId
idOf n p = case [ trackId t | t <- projectTracks p, trackScoreName t == n ] of
    (i : _) -> i
    [] -> error "no track"

-- | Apply a command at a time, recording it as the session does.
step :: (Project -> Change) -> (Project -> Command) -> Double -> (Project, History) -> (Project, History)
step describe cmd now (p, h) =
    let p' = either (error . show) id (applyCommand (cmd p) p)
    in (p', record now (describe p) p p' h)

typing :: Text -> Double -> (Project, History) -> (Project, History)
typing t = step (const (Change "typing" (Just "score") 2)) (const (SetScoreText t))

gain :: Text -> Double -> Double -> (Project, History) -> (Project, History)
gain n g = step (const (Change "level" (Just ("gain:" <> n)) 1.5)) (\p -> UpdateTrackMix (idOf n p) noMixUpdate { updateGain = Just g })

mute :: Text -> Bool -> Double -> (Project, History) -> (Project, History)
mute n m = step (const (Change "mute" Nothing 0)) (\p -> UpdateTrackMix (idOf n p) noMixUpdate { updateMute = Just m })

sound :: Text -> Double -> (Project, History) -> (Project, History)
sound n = step (const (Change "sound" (Just ("patch:" <> n)) 1.5)) (\p -> SetTrackPatch (idOf n p) (fromJust (lookupPresetPatch "chime")))

undos :: Int -> (Project, History) -> [(Project, History)]
undos 0 _ = []
undos k (p, h) = case undo p h of
    Nothing -> []
    Just x -> x : undos (k - 1) x

historyTests :: IO Bool
historyTests = do
    putStrLn "\n--- History ---"
    let p0 = newProject score
        start = (p0, emptyHistory)
        s1 = typing (score <> "\n") 0 start
        s2 = sound "lead" 10 s1
        s3 = gain "bass" 0.5 20 s2
        s4 = mute "bass" True 30 s3
        states = [fst s4, fst s3, fst s2, fst s1, p0]
    rs <- sequence
        [ check "undo walks back through score, sound and mix changes, and redo forward" $ do
            let back = undos 10 s4
                forward = take 4 (iterateMaybe (\(p, h) -> redo p h) (last back))
            return (length back == 4 && and (zipWith sameContent (map fst back) (drop 1 states))
                    && and (zipWith sameContent (map fst forward) (drop 1 (reverse states))))
        , check "quick runs of the same change are one step; other changes and toggles are not" $ do
            let burst = typing "track x (lead):\n" 4.4 (typing "track x (lead)" 3.0 (typing "track x" 1.5 (typing "track" 0 start)))
                later = typing "track y (lead):\n    C4 whole\n" 9 burst
                nudges = gain "bass" 0.7 1.0 (gain "bass" 0.8 0.5 (gain "bass" 0.9 0 start))
                mixed = gain "lead" 0.7 0.6 (gain "bass" 0.8 0.5 start)
                toggles = mute "bass" False 0.2 (mute "bass" True 0 start)
            return (undoDepth (snd burst) == 1 && undoDepth (snd later) == 2
                    && undoDepth (snd nudges) == 1 && undoDepth (snd mixed) == 2 && undoDepth (snd toggles) == 2
                    && sameContent (fst (fromJust (uncurry undo burst))) p0)
        , check "one drag is one step: its single change undoes to before the drag" $ do
            let dragged = sound "lead" 0 start
            return (undoDepth (snd dragged) == 1 && sameContent (fst (fromJust (uncurry undo dragged))) p0)
        , check "a new change after undoing clears what could be redone" $ do
            (p, h) <- maybe (fail "nothing to undo") return (uncurry undo s4)
            let (p', h') = gain "lead" 0.3 40 (p, h)
            return (redoLabel h == Just "mute" && redoLabel h' == Nothing && undoLabel h' == Just "level"
                    && null (redo p' h'))
        , check "changes that change nothing are not steps" $ do
            let same = typing score 0 start
            return (undoDepth (snd same) == 0)
        , check "at most 200 steps are kept, the newest" $ do
            let many = foldl (\acc k -> mute "bass" (even k) (fromIntegral k) acc) start [1 .. 250 :: Int]
            return (undoDepth (snd many) == historyLimit && historyLimit == 200)
        ]
    return (and rs)
  where
    iterateMaybe f x = case f x of
        Nothing -> []
        Just y -> y : iterateMaybe f y
