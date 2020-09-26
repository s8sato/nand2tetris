{-# LANGUAGE OverloadedStrings #-}

module Parser ( aCommand, aLabel ) where

import Data.Attoparsec.Text
    ( endOfInput, decimal, char, peekChar', takeTill, Parser )
import Control.Applicative ( (<|>) )
import Data.Foldable ( asum )

import Lib.Label ( Label(..) )
import Lib.Command as Cmd ( Command(..) )
import Lib.Symbol as Symbol ( Symbol, new )
import Lib.Addr as Addr ( Addr, new )
import Lib.Dest as D ( Dest(..) )
import Lib.Comp as C ( Comp(..) )
import Lib.Jump as J ( Jump(..) )

aLabel :: Parser Label
aLabel = Label <$ char '(' <*> aSymbol <* char ')' <* endOfInput

aCommand :: Parser Command
aCommand =
        Cmd.V <$ char '@' <*> aSymbol <* endOfInput
    <|> Cmd.A <$ char '@' <*> aAddr   <* endOfInput
    <|> Cmd.C <$> (Just <$> aDest) <*> aComp <*> (Just <$> aJump)
    <|> Cmd.C <$> (Just <$> aDest) <*> aComp <*> pure Nothing
    <|> Cmd.C <$> pure Nothing     <*> aComp <*> (Just <$> aJump)

aSymbol :: Parser Symbol
aSymbol = Symbol.new =<< takeTill (== ')')

aAddr :: Parser Addr
aAddr = Addr.new =<< decimal

(><) ps xs = asum . concatMap (\p -> map ($ p) xs) $ ps

aDest :: Parser Dest
aDest =
    [ D.M   <$ "M"
    , D.D   <$ "D"
    , D.MD  <$ "MD"
    , D.A   <$ "A"
    , D.AM  <$ "AM"
    , D.AD  <$ "AD"
    , D.AMD <$ "AMD"
    ] >< [(<* char '=')]

aComp :: Parser Comp
aComp =
    [ C.O     <$ "0"
    , C.I     <$ "1"
    , C.DifOI <$ "-1"
    , C.D     <$ "D"
    , C.A     <$ "A"
    , C.NotD  <$ "!D"
    , C.NotA  <$ "!A"
    , C.DifOD <$ "-D"
    , C.DifOA <$ "-A"
    , C.AddDI <$ "D+1"
    , C.AddAI <$ "A+1"
    , C.DifDI <$ "D-1"
    , C.DifAI <$ "A-1"
    , C.AddDA <$ "D+A"
    , C.DifDA <$ "D-A"
    , C.DifAD <$ "A-D"
    , C.AndDA <$ "D&A"
    , C.OrDA  <$ "D|A"
    , C.M     <$ "M"
    , C.NotM  <$ "!M"
    , C.DifOM <$ "-M"
    , C.AddMI <$ "M+1"
    , C.DifMI <$ "M-1"
    , C.AddDM <$ "D+M"
    , C.DifDM <$ "D-M"
    , C.DifMD <$ "M-D"
    , C.AndDM <$ "D&M"
    , C.OrDM  <$ "D|M"
    ] >< [(<* (char ';' <* peekChar')), (<* endOfInput)]

aJump :: Parser Jump
aJump =
    [ J.JGT <$ "JGT"
    , J.JEQ <$ "JEQ"
    , J.JGE <$ "JGE"
    , J.JLT <$ "JLT"
    , J.JNE <$ "JNE"
    , J.JLE <$ "JLE"
    , J.JMP <$ "JMP"
    ] >< [(<* endOfInput)]
