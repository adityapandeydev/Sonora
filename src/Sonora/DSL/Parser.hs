{-# LANGUAGE FlexibleContexts #-}
module Sonora.DSL.Parser
    ( parseSonora
    , parseSonoraFile
    , pitchClassParser
    , durationParser
    ) where

import Control.Monad (void)
import Data.Char (toLower, toUpper)
import Data.Ratio ((%))
import Text.Parsec
import Text.Parsec.String (Parser)

import Sonora.DSL.AST
import Sonora.Music.Chord (ChordQuality (..))
import Sonora.Music.Duration
import Sonora.Music.Note (Articulation (..), Velocity (..), f, ff, fff, mf, mp, p, pp, ppp)
import Sonora.Music.Pitch (Octave (..), Pitch (..), PitchClass (..))

-- | Skip inline whitespace (spaces and tabs).
skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

-- | Skip comments starting with '#' or '//' until end of line.
comment :: Parser ()
comment = do
    void (string "#" <|> string "//")
    skipMany (noneOf "\r\n")

-- | Skip all whitespace and comments between statements.
ws :: Parser ()
ws = skipMany (void space <|> comment)

-- | Parse an identifier (letters, digits, underscores, dashes).
identifier :: Parser String
identifier = do
    c <- letter <|> char '_'
    cs <- many (alphaNum <|> char '_' <|> char '-')
    return (c:cs)

-- | Parse a floating point or integer number.
number :: Parser Double
number = do
    sign <- option "" (string "-")
    digits <- many1 digit
    mFrac <- optionMaybe (do
        d <- char '.'
        frac <- many1 digit
        return (d : frac))
    let numStr = sign ++ digits ++ maybe "" id mFrac
    return (read numStr)

-- | Parse an integer.
intNumber :: Parser Int
intNumber = do
    digits <- many1 digit
    return (read digits)

-- | Parse a pitch class like C4, F#3, Bb2, Eb5, G#0.
pitchClassParser :: Parser PitchClass
pitchClassParser = do
    pName <- oneOf "ABCDEFGabcdefg"
    mAcc  <- optionMaybe (oneOf "#bsf")
    oct   <- intNumber
    let basePitch = case toUpper pName of
            'C' -> C
            'D' -> D
            'E' -> E
            'F' -> F
            'G' -> G
            'A' -> A
            'B' -> B
            _   -> C
        finalPitch = case mAcc of
            Just '#' -> case basePitch of
                C -> Cs; D -> Ds; F -> Fs; G -> Gs; A -> As; _ -> basePitch
            Just 's' -> case basePitch of
                C -> Cs; D -> Ds; F -> Fs; G -> Gs; A -> As; _ -> basePitch
            Just 'b' -> case basePitch of
                D -> Cs; E -> Ds; G -> Fs; A -> Gs; B -> As; _ -> basePitch
            Just 'f' -> case basePitch of
                D -> Cs; E -> Ds; G -> Fs; A -> Gs; B -> As; _ -> basePitch
            _        -> basePitch
    return (PitchClass finalPitch (Octave oct))

-- | Parse a musical duration like "whole", "half", "quarter", "1/4", "dotted half".
durationParser :: Parser Duration
durationParser = try dottedParser <|> try fractionParser <|> wordDuration
  where
    dottedParser = do
        void (string "dotted" >> skipSpaces)
        base <- durationParser
        return (dotted base)

    fractionParser = do
        num <- intNumber
        void (char '/')
        den <- intNumber
        return (Duration (fromIntegral num % fromIntegral den))

    wordDuration = do
        w <- many1 letter
        case map toLower w of
            "doublewhole"  -> return doubleWhole
            "whole"        -> return whole
            "half"         -> return half
            "quarter"      -> return quarter
            "eighth"       -> return eighth
            "sixteenth"    -> return sixteenth
            "thirtysecond" -> return thirtySecond
            "sixtyfourth"  -> return sixtyFourth
            _              -> fail ("Unknown duration: " ++ w)

-- | Parse a velocity dynamic marking (p, mp, mf, f, etc.) or a number in [0.0, 1.0].
velocityParser :: Parser Velocity
velocityParser = try namedVel <|> numVel
  where
    namedVel = do
        w <- many1 letter
        case map toLower w of
            "ppp" -> return ppp
            "pp"  -> return pp
            "p"   -> return p
            "mp"  -> return mp
            "mf"  -> return mf
            "f"   -> return f
            "ff"  -> return ff
            "fff" -> return fff
            _     -> fail ("Unknown velocity marking: " ++ w)
    numVel = do
        v <- number
        return (Velocity (max 0.0 (min 1.0 v)))

-- | Parse an articulation (normal, staccato, legato, tenuto).
articulationParser :: Parser Articulation
articulationParser = do
    w <- many1 letter
    case map toLower w of
        "staccato" -> return Staccato
        "legato"   -> return Legato
        "tenuto"   -> return Tenuto
        "normal"   -> return Normal
        _          -> fail ("Unknown articulation: " ++ w)

-- | Parse chord quality (maj, min, dim, aug, dom7, maj7, min7, sus2, sus4).
chordQualityParser :: Parser ChordQuality
chordQualityParser = do
    w <- many1 alphaNum
    case map toLower w of
        "maj"      -> return MajorTriad
        "major"    -> return MajorTriad
        "min"      -> return MinorTriad
        "minor"    -> return MinorTriad
        "dim"      -> return DiminishedTriad
        "aug"      -> return AugmentedTriad
        "dom7"     -> return Dominant7
        "maj7"     -> return Major7
        "min7"     -> return Minor7
        "halfdim7" -> return HalfDiminished7
        "dim7"     -> return Diminished7
        "sus2"     -> return Sus2
        "sus4"     -> return Sus4
        _          -> fail ("Unknown chord quality: " ++ w)

-- | Parse a single track item.
itemParser :: Parser ASTItem
itemParser = try repeatParser
         <|> try chordParser
         <|> try namedChordParser
         <|> try restParser
         <|> noteParser
  where
    restParser = do
        void (string "rest" >> skipSpaces)
        d <- durationParser
        return (ASTItemRest d)

    chordParser = do
        void (string "chord" >> skipSpaces >> char '(' >> skipSpaces)
        pitches <- sepBy1 pitchClassParser (skipSpaces >> char ',' >> skipSpaces)
        void (skipSpaces >> char ')' >> skipSpaces)
        dur <- durationParser
        mVel <- optionMaybe (skipSpaces >> velocityParser)
        return (ASTItemChord pitches dur mVel)

    namedChordParser = do
        root <- pitchClassParser
        skipSpaces
        quality <- chordQualityParser
        skipSpaces
        dur <- durationParser
        return (ASTItemNamedChord root quality dur)

    noteParser = do
        pc <- pitchClassParser
        skipSpaces
        dur <- durationParser
        mVel <- optionMaybe (try (skipSpaces >> velocityParser))
        mArt <- optionMaybe (try (skipSpaces >> articulationParser))
        return (ASTItemNote (ASTNote pc dur mVel mArt))

    repeatParser = do
        void (string "repeat" >> skipSpaces)
        n <- intNumber
        void (skipSpaces >> char ':' >> ws)
        items <- many1 (try (itemParser <* ws))
        return (ASTItemRepeat n items)

-- | Parse an instrument definition block.
instrumentParser :: Parser ASTInstrumentDef
instrumentParser = do
    void (string "instrument" >> skipSpaces)
    name <- identifier
    void (skipSpaces >> char ':' >> ws)

    -- Default values
    let parseOsc = do
            void (string "oscillator" >> optional (char ':') >> skipSpaces)
            many1 letter
        parseCutoff = do
            void (string "cutoff" >> optional (char ':') >> skipSpaces)
            number
        parseRes = do
            void (string "resonance" >> optional (char ':') >> skipSpaces)
            number
        parseEnv = do
            void (string "envelope" >> optional (char ':') >> skipSpaces)
            a <- number <* skipSpaces
            d <- number <* skipSpaces
            s <- number <* skipSpaces
            r <- number
            return (a, d, s, r)

    osc <- option "saw" (try parseOsc <* ws)
    fc  <- option 2500.0 (try parseCutoff <* ws)
    res <- option 1.0 (try parseRes <* ws)
    adsrVals <- option (0.01, 0.1, 0.7, 0.2) (try parseEnv <* ws)

    return (ASTInstrumentDef name osc fc res adsrVals)

-- | Parse a track definition block.
trackParser :: Parser ASTTrack
trackParser = do
    void (string "track" >> skipSpaces)
    tName <- identifier
    skipSpaces
    mInst <- optionMaybe (do
        void (char '(' >> skipSpaces)
        i <- identifier
        void (skipSpaces >> char ')')
        return i)
    void (skipSpaces >> char ':' >> ws)
    let instName = maybe tName id mInst

    items <- many1 (try (itemParser <* ws))
    return (ASTTrack tName instName items)

-- | Parse top-level directives (tempo, time, title).
directiveParser :: Parser (Either (String, String) (ASTSong -> ASTSong))
directiveParser = (try tempoDir <|> try timeDir <|> titleDir) <* ws
  where
    tempoDir = do
        void (string "tempo" >> skipSpaces)
        bpmVal <- number
        return (Right $ \s -> s { astSongBPM = Just (BPM bpmVal) })

    timeDir = do
        void (string "time" >> skipSpaces)
        num <- intNumber
        void (char '/')
        den <- intNumber
        return (Right $ \s -> s { astSongTimeSig = Just (TimeSignature num den) })

    titleDir = do
        void (string "title" >> optional (char ':') >> skipSpaces)
        mQuote <- optionMaybe (char '"')
        case mQuote of
            Just _ -> do
                t <- many (noneOf "\"\r\n")
                void (char '"')
                return (Right $ \s -> s { astSongTitle = t })
            Nothing -> do
                t <- many (noneOf "\r\n")
                return (Right $ \s -> s { astSongTitle = t })

-- | Top-level parser for complete .sonora documents.
songParser :: Parser ASTSong
songParser = do
    ws
    let initSong = ASTSong "Untitled" Nothing Nothing [] []
    songWithDirectives <- parseElements initSong
    eof
    return songWithDirectives
  where
    parseElements currentSong = do
        mDir <- optionMaybe (try directiveParser)
        case mDir of
            Just (Right updateFn) -> parseElements (updateFn currentSong)
            _ -> do
                mInst <- optionMaybe (try instrumentParser <* ws)
                case mInst of
                    Just inst -> parseElements (currentSong { astSongInstruments = astSongInstruments currentSong ++ [inst] })
                    Nothing -> do
                        mTrack <- optionMaybe (try trackParser <* ws)
                        case mTrack of
                            Just track -> parseElements (currentSong { astSongTracks = astSongTracks currentSong ++ [track] })
                            Nothing -> return currentSong

-- | Parse a Sonora source string.
parseSonora :: String -> String -> Either ParseError ASTSong
parseSonora srcName input = parse songParser srcName input

-- | Parse a Sonora file from disk.
parseSonoraFile :: FilePath -> IO (Either ParseError ASTSong)
parseSonoraFile path = do
    content <- readFile path
    return (parseSonora path content)
