{-# LANGUAGE DeriveGeneric #-}
module Sonora.DSL.Lexer
    ( Token (..)
    , tokenize
    ) where

import Data.Char (isAlphaNum, isSpace, toLower)
import GHC.Generics (Generic)

-- | Lexical tokens recognized by the Sonora score scanner.
data Token
    = TokDirective !String
    | TokKeyword !String
    | TokIdent !String
    | TokNumber !Double
    | TokColon
    | TokLParen
    | TokRParen
    | TokComma
    | TokNewline
    | TokEOF
    deriving (Eq, Show, Generic)

-- | Simple lexical scanner for tokenizing Sonora source code into a stream of tokens.
tokenize :: String -> [Token]
tokenize [] = [TokEOF]
tokenize (c:cs)
    | isSpace c && (c == ' ' || c == '\t') = tokenize cs
    | c == '\r' || c == '\n'               = TokNewline : tokenize cs
    | c == '#' || (c == '/' && take 1 cs == "/") =
        let rest = dropWhile (\x -> x /= '\r' && x /= '\n') cs
        in tokenize rest
    | c == ':' = TokColon : tokenize cs
    | c == '(' = TokLParen : tokenize cs
    | c == ')' = TokRParen : tokenize cs
    | c == ',' = TokComma : tokenize cs
    | c `elem` ("0123456789-" :: String) && not (null (filter (`elem` ("0123456789" :: String)) [c])) =
        let (numStr, rest) = span (\x -> x `elem` ("0123456789." :: String)) (c:cs)
        in case reads numStr of
            [(n, "")] -> TokNumber n : tokenize rest
            _         -> TokIdent numStr : tokenize rest
    | isAlphaNum c || c == '_' =
        let (ident, rest) = span (\x -> isAlphaNum x || x `elem` ("_#-+" :: String)) (c:cs)
            lower = map toLower ident
            tok = case lower of
                "tempo"      -> TokKeyword "tempo"
                "time"       -> TokKeyword "time"
                "title"      -> TokKeyword "title"
                "track"      -> TokKeyword "track"
                "instrument" -> TokKeyword "instrument"
                "repeat"     -> TokKeyword "repeat"
                "rest"       -> TokKeyword "rest"
                "chord"      -> TokKeyword "chord"
                _            -> TokIdent ident
        in tok : tokenize rest
    | otherwise = tokenize cs
