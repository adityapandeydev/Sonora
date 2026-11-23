{-# LANGUAGE OverloadedStrings #-}
-- | Where things are in a score: which lines belong to which track, and
-- when each line sounds.
--
-- The outline is read from the text as it stands, line by line, so it is
-- available while the score has errors. Timing comes from the last valid
-- score and its line numbers only match the text while the score is current.
module Sonora.Project.Layout
    ( Outline (..)
    , OutlineTrack (..)
    , outline
    , SongInfo (..)
    , songInfo
    , LineTiming (..)
    , NoteEvent (..)
    , TrackTiming (..)
    , timing
    ) where

import Data.Aeson
import Data.Char (isAlphaNum)
import Data.List (mapAccumL)
import qualified Data.Text as T
import Data.Text (Text)

import Sonora.DSL.AST
import Sonora.DSL.Evaluator (evaluateItem)
import Sonora.Music.Composition (TimedEvent (..), linearize, musicDuration)
import Sonora.Music.Duration (BPM (..), Duration (..), TimeSignature (..), defaultBPM, defaultTimeSignature, durationToSeconds)
import Sonora.Music.Note (Note (..))
import Sonora.Music.Pitch (pitchToMidi)
import Sonora.Project.Types

-- | A track block in the text: its header line and its last line (1-based,
-- inclusive). Blank lines between blocks belong to the block above.
data OutlineTrack = OutlineTrack
    { outlineName   :: !Text
    , outlineHeader :: !Int
    , outlineLast   :: !Int
    } deriving (Eq, Show)

data Outline = Outline
    { outlineLines  :: !Int            -- ^ Number of lines in the text.
    , outlineTracks :: ![OutlineTrack]
    } deriving (Eq, Show)

instance ToJSON OutlineTrack where
    toJSON (OutlineTrack n h l) = object ["name" .= n, "header" .= h, "last" .= l]

instance ToJSON Outline where
    toJSON (Outline n ts) = object ["lines" .= n, "tracks" .= ts]

-- | Track blocks by their @track <name>@ headers. Lenient: any line whose
-- first word is @track@ starts a block, whether or not the rest parses.
outline :: Text -> Outline
outline txt =
    let ls = T.splitOn "\n" txt
        n = length ls
        headers = [ (i, name) | (i, l) <- zip [1 ..] ls, Just name <- [headerName l] ]
        lasts = map (subtract 1 . fst) (drop 1 headers) ++ [n]
    in Outline n [ OutlineTrack name h end | ((h, name), end) <- zip headers lasts ]
  where
    headerName l = case T.words (T.takeWhile (/= '#') l) of
        (w : rest) | w == "track" ->
            Just (T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') (T.concat (take 1 rest)))
        _ -> Nothing

data SongInfo = SongInfo
    { infoTitle    :: !Text
    , infoBPM      :: !Double
    , infoTimeSig  :: !(Int, Int)
    , infoBarBeats :: !Double  -- ^ Quarter-note beats per bar.
    } deriving (Eq, Show)

instance ToJSON SongInfo where
    toJSON (SongInfo t b (n, d) bb) = object
        ["title" .= t, "bpm" .= b, "timeSignature" .= [n, d], "beatsPerBar" .= bb]

songInfo :: ASTSong -> SongInfo
songInfo ast =
    let BPM b = maybe defaultBPM id (astSongBPM ast)
        TimeSignature n d = maybe defaultTimeSignature id (astSongTimeSig ast)
    in SongInfo (T.pack (astSongTitle ast)) b (n, d) (4 * fromIntegral n / fromIntegral (max 1 d))

-- | When one line of the score sounds, in seconds, and the quarter-note
-- beat it starts on.
data LineTiming = LineTiming
    { lineNumber :: !Int
    , lineStart  :: !Double
    , lineEnd    :: !Double
    , lineBeat   :: !Double
    } deriving (Eq, Show)

-- | A sounding note: start and end in seconds, MIDI pitch, source line.
data NoteEvent = NoteEvent !Double !Double !Int !Int
    deriving (Eq, Show)

data TrackTiming = TrackTiming
    { timingTrack :: !TrackId
    , timingLines :: ![LineTiming]
    , timingNotes :: ![NoteEvent]
    } deriving (Eq, Show)

instance ToJSON LineTiming where
    toJSON (LineTiming l s e b) = toJSON [fromIntegral l, s, e, b]

instance ToJSON NoteEvent where
    toJSON (NoteEvent s e p l) = toJSON [s, e, fromIntegral p, fromIntegral l]

instance ToJSON TrackTiming where
    toJSON (TrackTiming t ls ns) = object ["track" .= t, "lines" .= ls, "notes" .= ns]

-- | Timing of every item line of the last valid score, by stable track id.
-- A @repeat@ block is attributed to the line it starts on.
timing :: Project -> Maybe [TrackTiming]
timing p = do
    ValidScore _ ast <- projectValidScore p
    let bpm = maybe defaultBPM id (astSongBPM ast)
        secs = durationToSeconds bpm
        beats (Duration d) = 4 * fromRational d :: Double
        track ts astT =
            let items = zip (astTrackItemLines astT) (astTrackItems astT)
                step start (line, item) =
                    let music = evaluateItem item
                        len = musicDuration noteDuration music
                        notes = [ NoteEvent (secs (start + eventStart e)) (secs (start + eventStart e + eventDuration e))
                                            (pitchToMidi (notePitch (eventValue e))) line
                                | e <- linearize noteDuration music ]
                    in (start + len, (LineTiming line (secs start) (secs (start + len)) (beats start), notes))
                (_, steps) = mapAccumL step 0 items
            in TrackTiming (trackId ts) (map fst steps) (concatMap snd steps)
    return (zipWith track (projectTracks p) (astSongTracks ast))
