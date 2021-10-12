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
import Lib.Comp as C ( Comp(..), IR(..), R(..), AM(..) )
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
    [ C.O       <$ char '0'
    , C.Id      <$> aIR
    , C.Neg     <$ char '-' <*> aIR
    , C.Not     <$ char '!' <*> aR
    , C.Inc     <$> aR <* "+1"
    , C.Dec     <$> aR <* "-1"
    , C.AddD    <$ "D+" <*> aAM
    , C.SubD    <$ "D-" <*> aAM
    , C.SubXD   <$> aAM <* "-D"
    , C.AndD    <$ "D&" <*> aAM
    , C.OrD     <$ "D|" <*> aAM
    ] >< [(<* (char ';' <* peekChar')), (<* endOfInput)]

aIR :: Parser IR
aIR =   C.I <$  char '1'
    <|> C.R <$> aR

aR :: Parser R
aR =    C.D  <$  char 'D'
    <|> C.AM <$> aAM

aAM :: Parser AM
aAM =   C.A <$ char 'A'
    <|> C.M <$ char 'M'

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
