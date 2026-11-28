{-# LANGUAGE OverloadedStrings #-}
-- | Undo and redo for a project.
--
-- Each step keeps the whole project as it was before a change (projects
-- share most of their structure, so this is cheap). Changes of the same
-- kind that follow each other quickly form one step: a burst of typing, or
-- repeated nudges of one control. Toggles and whole-score replacements are
-- always steps of their own. At most 'historyLimit' steps are kept.
module Sonora.Project.History
    ( History
    , emptyHistory
    , historyLimit
    , Change (..)
    , record
    , undo
    , redo
    , undoLabel
    , redoLabel
    , undoDepth
    , sameContent
    ) where

import Data.Text (Text)

import Sonora.Project.Types

data Change = Change
    { changeLabel :: !Text          -- ^ What undoing it does, e.g. "typing".
    , changeGroup :: !(Maybe Text)  -- ^ Changes in the same group may merge.
    , changeMerge :: !Double        -- ^ Seconds within which they merge.
    } deriving (Eq, Show)

data Step = Step
    { stepBefore :: !Project
    , stepChange :: !Change
    , stepAt     :: !Double   -- ^ When the step was last extended.
    }

data History = History
    { past   :: ![Step]   -- ^ Newest first.
    , future :: ![Step]   -- ^ Next to redo first.
    }

historyLimit :: Int
historyLimit = 200

emptyHistory :: History
emptyHistory = History [] []

-- | Whether two projects hold the same work (revisions aside).
sameContent :: Project -> Project -> Bool
sameContent a b = a { projectRevision = Revision 0 } == b { projectRevision = Revision 0 }

-- | Note a change from @before@ to @after@ at a time in seconds. Nothing is
-- recorded when nothing changed. Any new change clears what could be redone.
record :: Double -> Change -> Project -> Project -> History -> History
record now change before after h
    | sameContent before after = h
    | otherwise = case past h of
        (s : rest)
            | Just g <- changeGroup change
            , changeGroup (stepChange s) == Just g
            , now - stepAt s <= changeMerge change
            -> History (s { stepAt = now } : rest) []
        _ -> History (take historyLimit (Step before change now : past h)) []

-- | Go back one step from the current project: the project to show, and the
-- history after. Nothing when there is nothing to undo.
undo :: Project -> History -> Maybe (Project, History)
undo current h = case past h of
    [] -> Nothing
    (s : rest) -> Just (stepBefore s, History rest (s { stepBefore = current } : future h))

redo :: Project -> History -> Maybe (Project, History)
redo current h = case future h of
    [] -> Nothing
    (s : rest) -> Just (stepBefore s, History (s { stepBefore = current } : past h) rest)

undoLabel, redoLabel :: History -> Maybe Text
undoLabel = fmap (changeLabel . stepChange) . headMaybe . past
redoLabel = fmap (changeLabel . stepChange) . headMaybe . future

undoDepth :: History -> Int
undoDepth = length . past

headMaybe :: [a] -> Maybe a
headMaybe (x : _) = Just x
headMaybe [] = Nothing
