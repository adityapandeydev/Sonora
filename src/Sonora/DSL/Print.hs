{-# LANGUAGE OverloadedStrings #-}
-- | Write a parsed score back out as .sonora text.
--
-- The output parses back to the same song: durations are exact (words,
-- @dotted@ words, or @n/d@), velocities and numbers are printed in their
-- shortest exactly-readable decimal form, and chords stay chords. Comments
-- are not part of the syntax tree; the printer adds a @# Bar n@ comment
-- before the first item that starts each bar.
module Sonora.DSL.Print
    ( printSong
    , printPitch
    , printDuration
    ) where

import Data.List (intercalate)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showFFloat)

import Sonora.DSL.AST
import Sonora.Music.Chord (ChordQuality (..))
import Sonora.Music.Duration
import Sonora.Music.Note (Articulation (..), Velocity (..))
import Sonora.Music.Pitch (Octave (..), Pitch (..), PitchClass (..))

printSong :: ASTSong -> Text
printSong s = T.pack . unlines $
    [ "title \"" ++ filter (`notElem` ("\"\r\n" :: String)) (astSongTitle s) ++ "\"" ]
    ++ maybe [] (\(BPM b) -> ["tempo " ++ number b]) (astSongBPM s)
    ++ maybe [] (\(TimeSignature n d) -> ["time " ++ show n ++ "/" ++ show d]) (astSongTimeSig s)
    ++ concatMap (\i -> "" : instrument i) (astSongInstruments s)
    ++ concatMap (\t -> "" : track barLength t) (astSongTracks s)
  where
    barLength = case astSongTimeSig s of
        Just (TimeSignature n d) | n > 0 && d > 0 -> fromIntegral n % fromIntegral d
        _ -> 1

instrument :: ASTInstrumentDef -> [String]
instrument i =
    let (a, d, s, r) = astInstADSR i
    in [ "instrument " ++ astInstName i ++ ":"
       , "    oscillator " ++ astInstOsc i
       , "    cutoff " ++ number (astInstCutoff i)
       , "    resonance " ++ number (astInstRes i)
       , "    envelope " ++ unwords (map number [a, d, s, r])
       ]

track :: Rational -> ASTTrack -> [String]
track barLength t =
    ("track " ++ astTrackName t ++ " (" ++ astTrackInstrument t ++ "):")
        : go 0 (astTrackItems t)
  where
    go _ [] = []
    go at (item : rest) =
        let bar = floor (at / barLength) :: Integer
            starts = at == fromIntegral bar * barLength
            label = [ "    # Bar " ++ show (bar + 1) | starts ]
        in label ++ itemLines 1 item ++ go (at + itemLength item) rest

itemLength :: ASTItem -> Rational
itemLength item = case item of
    ASTItemNote n             -> toRational (astDuration n)
    ASTItemRest d             -> toRational d
    ASTItemChord _ d _        -> toRational d
    ASTItemNamedChord _ _ d   -> toRational d
    ASTItemRepeat k xs        -> fromIntegral k * sum (map itemLength xs)

itemLines :: Int -> ASTItem -> [String]
itemLines depth item =
    let pad = replicate (4 * depth) ' '
    in case item of
        ASTItemNote n -> [pad ++ unwords
            ( [printPitch (astPitch n), printDuration (astDuration n)]
              ++ maybe [] (\(Velocity v) -> [number v]) (astVelocity n)
              ++ maybe [] (\a -> [articulation a]) (astArticulation n) )]
        ASTItemRest d -> [pad ++ "rest " ++ printDuration d]
        ASTItemChord ps d v -> [pad ++ "chord(" ++ intercalate ", " (map printPitch ps) ++ ") "
            ++ printDuration d ++ maybe "" (\(Velocity x) -> ' ' : number x) v]
        ASTItemNamedChord p q d -> [pad ++ unwords [printPitch p, quality q, printDuration d]]
        ASTItemRepeat k xs -> (pad ++ "repeat " ++ show k ++ ":") : concatMap (itemLines (depth + 1)) xs

printPitch :: PitchClass -> String
printPitch (PitchClass p (Octave o)) = name ++ show o
  where
    name = case p of
        C -> "C"; Cs -> "C#"; D -> "D"; Ds -> "D#"; E -> "E"; F -> "F"
        Fs -> "F#"; G -> "G"; Gs -> "G#"; A -> "A"; As -> "A#"; B -> "B"

-- | Exact text for a duration: a word, a dotted word, or a fraction.
printDuration :: Duration -> String
printDuration (Duration r) = case lookup r words' of
    Just w -> w
    Nothing -> case lookup (r * (2 % 3)) words' of
        Just w -> "dotted " ++ w
        Nothing -> show (numerator r) ++ "/" ++ show (denominator r)
  where
    words' =
        [ (2, "doubleWhole"), (1, "whole"), (1 % 2, "half"), (1 % 4, "quarter")
        , (1 % 8, "eighth"), (1 % 16, "sixteenth"), (1 % 32, "thirtysecond")
        , (1 % 64, "sixtyfourth") ]

articulation :: Articulation -> String
articulation a = case a of
    Normal -> "normal"; Staccato -> "staccato"; Legato -> "legato"; Tenuto -> "tenuto"

quality :: ChordQuality -> String
quality q = case q of
    MajorTriad -> "maj"; MinorTriad -> "min"; DiminishedTriad -> "dim"; AugmentedTriad -> "aug"
    Dominant7 -> "dom7"; Major7 -> "maj7"; Minor7 -> "min7"; HalfDiminished7 -> "halfdim7"
    Diminished7 -> "dim7"; Sus2 -> "sus2"; Sus4 -> "sus4"

-- | Shortest decimal that reads back exactly; whole numbers without ".0".
number :: Double -> String
number x
    | x == fromIntegral r && abs x < 1e15 = show r
    | otherwise = showFFloat Nothing x ""
  where
    r = round x :: Integer
