{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseLines
    ) where

import Control.Applicative              ( (<|>) )
import Data.Char                        ( isSpace )
import Data.Foldable                    ( asum )
import Data.Attoparsec.Text             ( Parser
                                        , char
                                        , decimal
                                        , endOfInput
                                        , parseOnly
                                        , peekChar'
                                        , takeTill
                                        )
import qualified Data.Text as T

import Lib.Command as Cmd               ( Command(..) )
import Lib.Symbol                       ( Symbol, makeSymbol )
import Lib.Addr                         ( Addr, makeAddr )
import Lib.Comp as C                    ( Comp(..) )
import Lib.Dest as D                    ( Dest(..) )
import Lib.Jump as J                    ( Jump(..) )
import Lib.Util                         ( mapSnd )

type Index = Int

parseLines :: T.Text -> [(Index, Either String Command)]
parseLines  = mapSnd (parseOnly aCommand)
            . extract . zip [1..] . T.lines

extract :: [(Index, T.Text)] -> [(Index, T.Text)]
extract = filter (\(_,t) -> not . T.null $ t)
        . mapSnd (fst . T.breakOn "//")
        . mapSnd (T.filter (not . isSpace))

aCommand :: Parser Command
aCommand =
        Cmd.L <$  char '('    <*> aSymbol <*  char ')' <* endOfInput
    <|> Cmd.V <$  char '@'    <*> aSymbol <*  endOfInput
    <|> Cmd.A <$  char '@'    <*> aAddr   <*  endOfInput
    <|> Cmd.C <$> aDest       <*> aComp   <*> aJump
    <|> Cmd.C <$> aDest       <*> aComp   <*> pure J.Null
    <|> Cmd.C <$> pure D.Null <*> aComp   <*> aJump

aSymbol :: Parser Symbol
aSymbol = do
    t <- takeTill (== ')')
    case makeSymbol t of
        Right s -> return s
        _       -> fail ""

aAddr :: Parser Addr
aAddr = do
    i <- decimal
    case makeAddr i of
        Right a -> return a
        _       -> fail ""

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
