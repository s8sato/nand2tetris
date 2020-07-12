{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseLines
    ) where

import Control.Applicative              ( (<|>) )
import Data.Attoparsec.Text             ( Parser
                                        , decimal
                                        , endOfInput
                                        , parseOnly
                                        , skipSpace
                                        , takeTill
                                        )
import qualified Data.Text as T

import Lib.Command
import Lib.Symbol
import Lib.Addr
import Lib.Arithmetic
import Lib.Segment
import Lib.Util

type Index = Int

parseLines :: T.Text -> [(Index, Either String Command)]
parseLines  = mapSnd (parseOnly aCommand)
            . extract . zip [1..] . T.lines

extract :: [(Index, T.Text)] -> [(Index, T.Text)]
extract = filter (\(_,t) -> not . T.null $ t)
        . mapSnd T.strip
        . mapSnd (fst . T.breakOn "//")

aCommand :: Parser Command
aCommand =
        Arithmetic <$> aArithmetic <* endOfInput
    <|> Push <$ "push" <* skipSpace <*> aSegment <* skipSpace <*> aAddr <* endOfInput
    <|> Pop  <$ "pop"  <* skipSpace <*> aSegment <* skipSpace <*> aAddr <* endOfInput
    <|> Label <$ "label"   <* skipSpace <*> aSymbol <* endOfInput
    <|> Goto  <$ "goto"    <* skipSpace <*> aSymbol <* endOfInput
    <|> If    <$ "if-goto" <* skipSpace <*> aSymbol <* endOfInput
    <|> Function <$ "function" <* skipSpace <*> aSymbol <* skipSpace <*> aAddr <* endOfInput
    <|> Call     <$ "call"     <* skipSpace <*> aSymbol <* skipSpace <*> aAddr <* endOfInput
    <|> Return <$ "return" <* endOfInput

aSymbol :: Parser Symbol
aSymbol = do
    t <- takeTill (== ' ')
    case makeSymbol t of
        Right s -> return s
        _       -> fail ""

aAddr :: Parser Addr
aAddr = do
    i <- decimal
    case makeAddr i of
        Right a -> return a
        _       -> fail ""

aArithmetic :: Parser Arithmetic
aArithmetic =
        Add <$ "add"
    <|> Sub <$ "sub"
    <|> Neg <$ "neg"
    <|> Eq  <$ "eq"
    <|> Gt  <$ "gt"
    <|> Lt  <$ "lt"
    <|> And <$ "and"
    <|> Or  <$ "or"
    <|> Not <$ "not"

aSegment :: Parser Segment
aSegment =
        Argument <$ "argument"
    <|> Local    <$ "local"
    <|> Static   <$ "static"
    <|> Constant <$ "constant"
    <|> This     <$ "this"
    <|> That     <$ "that"
    <|> Pointer  <$ "pointer"
    <|> Temp     <$ "temp"
