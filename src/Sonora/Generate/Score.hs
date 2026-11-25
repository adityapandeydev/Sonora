{-# LANGUAGE OverloadedStrings #-}
-- | Generate an editable score from a few settings.
--
-- The same settings always give the same score. Every track lasts exactly
-- the requested number of bars: each bar's rhythm is drawn so its durations
-- add up to one bar, and the last bar resolves to the tonic. The result is a
-- syntax tree, written out with "Sonora.DSL.Print", so what the user edits is
-- exactly what was generated.
module Sonora.Generate.Score
    ( GenSettings (..)
    , ScaleName
    , scaleNames
    , scaleTable
    , rootNames
    , rootTable
    , validateSettings
    , generateScore
    , generatedLength
    , settingsLabel
    ) where

import Data.Aeson
import Data.Char (toUpper)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import Sonora.DSL.AST
import Sonora.Generate.Random
import Sonora.Music.Chord (Chord (..), chordNotes)
import Sonora.Music.Duration
import Sonora.Music.Interval (Interval (..), transposePitch)
import Sonora.Music.Note (Velocity (..))
import Sonora.Music.Pitch (Octave (..), Pitch (..), PitchClass (..), pitchToMidi)
import Sonora.Music.Scale
import Sonora.Generate.Progression (diatonicChordOnDegree, generateProgression, voiceLead)

type ScaleName = Text

data GenSettings = GenSettings
    { genSeed  :: !Word64
    , genRoot  :: !Text       -- ^ One of 'rootNames'.
    , genScale :: !ScaleName  -- ^ One of 'scaleNames'.
    , genBPM   :: !Double
    , genBars  :: !Int
    } deriving (Eq, Show)

instance ToJSON GenSettings where
    toJSON g = object
        [ "seed" .= genSeed g, "root" .= genRoot g, "scale" .= genScale g
        , "bpm" .= genBPM g, "bars" .= genBars g ]

instance FromJSON GenSettings where
    parseJSON = withObject "settings" $ \o -> GenSettings
        <$> o .: "seed" <*> o .: "root" <*> o .: "scale" <*> o .: "bpm" <*> o .: "bars"

-- | Scales on offer, by wire name. Chromatic is left out: it has no key.
scaleTable :: [(ScaleName, ScaleType)]
scaleTable =
    [ ("major", Major), ("naturalMinor", NaturalMinor), ("harmonicMinor", HarmonicMinor)
    , ("melodicMinor", MelodicMinor), ("dorian", Dorian), ("phrygian", Phrygian)
    , ("lydian", Lydian), ("mixolydian", Mixolydian), ("locrian", Locrian)
    , ("majorPentatonic", MajorPentatonic), ("minorPentatonic", MinorPentatonic)
    , ("blues", Blues) ]

scaleNames :: [ScaleName]
scaleNames = map fst scaleTable

rootTable :: [(Text, Pitch)]
rootTable =
    [ ("C", C), ("C#", Cs), ("D", D), ("D#", Ds), ("E", E), ("F", F)
    , ("F#", Fs), ("G", G), ("G#", Gs), ("A", A), ("A#", As), ("B", B) ]

rootNames :: [Text]
rootNames = map fst rootTable

-- | Seconds of music the settings ask for (4/4 throughout).
generatedLength :: GenSettings -> Double
generatedLength g = fromIntegral (genBars g * 4) * 60 / genBPM g

-- | Problems with the settings, as (parameter, message) pairs.
validateSettings :: GenSettings -> [(Text, Text)]
validateSettings g = concat
    [ [ ("seed", "Seed must be between 0 and 4294967295") | genSeed g > 4294967295 ]
    , [ ("root", "Unknown root " <> genRoot g) | lookup (genRoot g) rootTable == Nothing ]
    , [ ("scale", "Unknown scale " <> genScale g) | lookup (genScale g) scaleTable == Nothing ]
    , [ ("bpm", "Tempo must be between 40 and 240 bpm") | not (genBPM g >= 40 && genBPM g <= 240) ]
    , [ ("bars", "Length must be between 1 and 64 bars") | genBars g < 1 || genBars g > 64 ]
    , [ ("bars", "The song would last over three minutes; use fewer bars or a faster tempo")
      | genBars g >= 1, genBPM g >= 40, generatedLength g > 180 ]
    ]

-- | A short description, such as "D dorian, 98 bpm, 8 bars, seed 42".
settingsLabel :: GenSettings -> Text
settingsLabel g = T.concat
    [ genRoot g, " ", spaced (genScale g), ", ", T.pack (showBPM (genBPM g)), " bpm, "
    , T.pack (show (genBars g)), " bar", if genBars g == 1 then "" else "s"
    , ", seed ", T.pack (show (genSeed g)) ]
  where
    spaced = T.toLower . T.concatMap (\c -> if c == toUpper c && c /= ' ' then T.pack [' ', c] else T.singleton c)
    showBPM b = if b == fromIntegral (round b :: Int) then show (round b :: Int) else show b

-- | The score for valid settings ('validateSettings' returns nothing).
generateScore :: GenSettings -> ASTSong
generateScore g = ASTSong
    { astSongTitle = T.unpack ("Generated: " <> settingsLabel g)
    , astSongBPM = Just (BPM (genBPM g))
    , astSongTimeSig = Just (TimeSignature 4 4)
    , astSongInstruments = []
    , astSongTracks =
        [ ASTTrack "lead" "lead" leadItems []
        , ASTTrack "chords" "pad" padItems []
        , ASTTrack "bass" "bass" bassItems []
        ]
    }
  where
    bars = genBars g
    sType = maybe Major id (lookup (genScale g) scaleTable)
    pitch = maybe C id (lookup (genRoot g) rootTable)
    -- Keep every key in the same register: roots from F# upward sit an octave down.
    chordOctave = if fromEnum pitch >= fromEnum Fs then 3 else 4
    chordRoot = PitchClass pitch (Octave chordOctave)
    melodyScale = Scale (PitchClass pitch (Octave (chordOctave + 1))) sType
    harmonyScale = Scale chordRoot (harmonyType sType)

    seeds = streams (Seed (genSeed g))
    -- One chord per bar, closing on the tonic, voiced to move as little as possible.
    tonic = diatonicChordOnDegree harmonyScale 1
    progression = voiceLead (init (generateProgression (seeds !! 0) harmonyScale bars) ++ [tonic])

    padItems = [ ASTItemChord (chordNotes c) whole (Just (Velocity 0.5)) | c <- progression ]
    bassItems = bassLine (seeds !! 1) progression
    leadItems = melody (seeds !! 2) melodyScale progression

-- | Independent generators derived from one seed.
streams :: Seed -> [Seed]
streams s = go (mkPRNG s)
  where go p = let (w, p') = nextWord64 p in Seed w : go p'

-- | Pentatonic and blues scales have no stacked thirds; harmonise with the parent mode.
harmonyType :: ScaleType -> ScaleType
harmonyType t = case t of
    MajorPentatonic -> Major
    MinorPentatonic -> NaturalMinor
    Blues           -> NaturalMinor
    Chromatic       -> Major
    other           -> other

-- | One pattern for the whole song, ending on a held root.
bassLine :: Seed -> [Chord] -> [ASTItem]
bassLine seed chords =
    let (pattern, _) = chooseRandom patterns (mkPRNG seed)
        lastBar = length chords - 1
    in concat [ if i == lastBar then [note (bassRoot c) whole 0.8] else pattern c | (i, c) <- zip [0 :: Int ..] chords ]
  where
    bassRoot c = let r = transposePitch (Interval (-24)) (chordRoot c)
                 in if pitchToMidi r < 28 then transposePitch (Interval 12) r else r
    fifth c = transposePitch (Interval 7) (bassRoot c)
    octave c = transposePitch (Interval 12) (bassRoot c)
    patterns =
        [ \c -> replicate 8 (note (bassRoot c) eighth 0.8)
        , \c -> [note (bassRoot c) half 0.85, note (fifth c) half 0.75]
        , \c -> replicate 4 (note (bassRoot c) quarter 0.8)
        , \c -> [note (bassRoot c) (dotted quarter) 0.85, note (bassRoot c) eighth 0.7, note (fifth c) half 0.75]
        , \c -> concat (replicate 2 [note (bassRoot c) eighth 0.85, note (octave c) eighth 0.7, note (fifth c) quarter 0.75])
        ]

note :: PitchClass -> Duration -> Double -> ASTItem
note p d v = ASTItemNote (ASTNote p d (Just (Velocity v)) Nothing)

-- | A stepwise melody. Rhythms come in two-bar phrases that repeat with new
-- pitches; each bar starts on a chord tone; the last bar holds the tonic.
melody :: Seed -> Scale -> [Chord] -> [ASTItem]
melody seed scl chords = go (mkPRNG seed) (min 5 span') (zip [0 :: Int ..] chords) []
  where
    bars = length chords
    -- Degrees within an octave and a half of the root, whatever the scale's size.
    rootMidi = pitchToMidi (scaleDegree scl 1)
    span' = length (takeWhile (\d -> pitchToMidi (scaleDegree scl d) - rootMidi <= 19) [1 ..])
    -- The tonic and its octave: degree 1 and degree (notes in the scale + 1).
    tonics = [1, length (scaleIntervals (scaleType scl)) + 1]
    go _ _ [] _ = []
    go p deg ((i, c) : rest) phrase
        | i == bars - 1 = [note (scaleDegree scl (minimumBy (comparing (\t -> abs (t - deg))) tonics)) whole 0.7]
        | otherwise =
            let (rhythm, p1, phrase') = barRhythm p i phrase
                (items, deg', p2) = pitches p1 deg c rhythm
            in items ++ go p2 deg' rest phrase'

    -- Bars 0 and 1 of each phrase draw rhythms; later phrases reuse them,
    -- occasionally varying one bar.
    barRhythm p i phrase
        | i < 2 = let (r, p') = rhythmFor p in (r, p', phrase ++ [r])
        | otherwise =
            let (roll, p1) = nextIntRange (1, 100) p
                stored = phrase !! (i `mod` 2)
            in if roll <= 25 then let (r, p2) = rhythmFor p1 in (r, p2, phrase) else (stored, p1, phrase)

    -- A bar is a run of rhythm cells, each a whole number of beats long, so
    -- every beat starts a new cell and nothing drifts off the beat.
    rhythmFor p = fill p (1 :: Rational) []
    fill p left acc
        | left <= 0 = (concat (reverse acc), p)
        | otherwise =
            let fits = [ (cell, w) | (cell, w) <- cells, cellLength cell <= left ]
                (pick, p') = weighted fits p
            in if null fits then (concat (reverse ([Duration left] : acc)), p)
               else fill p' (left - cellLength pick) (pick : acc)
    cellLength = sum . map toRational
    cells =
        [ ([quarter], 40)
        , ([eighth, eighth], 26)
        , ([half], 14)
        , ([dotted quarter, eighth], 12)
        , ([sixteenth, sixteenth, eighth], 4)
        , ([eighth, quarter, eighth], 4)
        ]

    pitches p deg c rhythm = step p deg rhythm (True :: Bool)
      where
        tones = map (`mod` 12) (map pitchToMidi (chordNotes c))
        chordDegrees = [ d | d <- [1 .. span'], (pitchToMidi (scaleDegree scl d) `mod` 12) `elem` tones ]
        step g d [] _ = ([], d, g)
        step g d (dur : ds) first =
            let (roll, g1) = nextIntRange (1, 100) g
                delta | roll <= 35 = 1 | roll <= 70 = -1 | roll <= 82 = 2 | roll <= 94 = -2 | otherwise = 0
                moved = max 1 (min span' (d + delta))
                d' = if first && not (null chordDegrees)
                        then minimumBy (comparing (\x -> abs (x - moved))) chordDegrees
                        else moved
                (restRoll, g2) = nextIntRange (1, 100) g1
                (vel, g3) = nextIntRange (62, 88) g2
                item | not first && restRoll <= 8 = ASTItemRest dur
                     | otherwise = note (scaleDegree scl d') dur (fromIntegral vel / 100)
                (more, dEnd, gEnd) = step g3 d' ds False
            in (item : more, dEnd, gEnd)

weighted :: [(a, Int)] -> PRNG -> (a, PRNG)
weighted xs p =
    let total = sum (map snd xs)
        (r, p') = nextIntRange (1, total) p
        pick n ((x, w) : rest) = if n <= w then x else pick (n - w) rest
        pick _ [] = fst (last xs)
    in (pick r xs, p')
