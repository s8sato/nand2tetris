{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseLines
    ) where

import Control.Applicative              ( (<|>) )
import Data.Char                        ( isSpace )
import Data.Foldable                    ( asum )
import Data.Ix                          ( inRange )
import Data.Attoparsec.Text             ( Parser
                                        , char
                                        , decimal
                                        , endOfInput
                                        , parseOnly
                                        , peekChar'
                                        )
import qualified Data.Text as T

import Lib.Command as Cmd
import Lib.Natural15
import Lib.Comp as C
import Lib.Dest as D
import Lib.Jump as J

mapSnd f = map (\(a, b) -> (a, f b))

parseLines :: T.Text -> [(Int, Either String Command)]
parseLines  = mapSnd (parseOnly aCommand)
            . extract
            . zip [1..]
            . T.lines

extract :: [(Int, T.Text)] -> [(Int, T.Text)]
extract = filter (\(_, t) -> not . T.null $ t)
        . mapSnd (fst . T.breakOn "//")
        . mapSnd (T.filter (not . isSpace))

aCommand :: Parser Command
aCommand =
        Cmd.A <$  char '@'    <*> aAddr <*  endOfInput
    <|> Cmd.C <$> aDest       <*> aComp <*> aJump
    <|> Cmd.C <$> aDest       <*> aComp <*> pure J.Null
    <|> Cmd.C <$> pure D.Null <*> aComp <*> aJump

aAddr :: Parser Natural15
aAddr = do
    i <- decimal
    let min = toInteger (minBound :: Natural15)
    let max = toInteger (maxBound :: Natural15)
    if inRange (min, max) i
        then return (fromInteger i)
        else fail ""

(><) ps xs = asum . concatMap (\p -> map ($ p) xs) $ ps

aDest :: Parser Dest
aDest =
    [ D.M <$ "M"
    , D.D <$ "D"
    , MD  <$ "MD"
    , D.A <$ "A"
    , AM  <$ "AM"
    , AD  <$ "AD"
    , AMD <$ "AMD"
    ] >< [(<* char '=')]

aComp :: Parser Comp
aComp =
    [ O     <$ "0"
    , I     <$ "1"
    , DifOI <$ "-1"
    , C.D   <$ "D"
    , C.A   <$ "A"
    , NotD  <$ "!D"
    , NotA  <$ "!A"
    , DifOD <$ "-D"
    , DifOA <$ "-A"
    , AddDI <$ "D+1"
    , AddAI <$ "A+1"
    , DifDI <$ "D-1"
    , DifAI <$ "A-1"
    , AddDA <$ "D+A"
    , DifDA <$ "D-A"
    , DifAD <$ "A-D"
    , AndDA <$ "D&A"
    , OrDA  <$ "D|A"
    , C.M   <$ "M"
    , NotM  <$ "!M"
    , DifOM <$ "-M"
    , AddMI <$ "M+1"
    , DifMI <$ "M-1"
    , AddDM <$ "D+M"
    , DifDM <$ "D-M"
    , DifMD <$ "M-D"
    , AndDM <$ "D&M"
    , OrDM  <$ "D|M"
    ] >< [(<* (char ';' <* peekChar')), (<* endOfInput)]

aJump :: Parser Jump
aJump =
    [ JGT <$ "JGT"
    , JEQ <$ "JEQ"
    , JGE <$ "JGE"
    , JLT <$ "JLT"
    , JNE <$ "JNE"
    , JLE <$ "JLE"
    , JMP <$ "JMP"
    ] >< [(<* endOfInput)]
