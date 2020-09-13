{-# LANGUAGE OverloadedStrings #-}

module Encoder
    ( Binary
    , encode
    ) where

import Numeric                          ( showIntAtBase )
import Data.Char                        ( intToDigit )
import qualified Data.Text as T

import Lib.Command as Cmd               ( Command(C, A) )
import Lib.Addr                         ( Addr )
import Lib.Comp as C                    ( Comp(..) )
import Lib.Dest as D                    ( Dest(..) )
import Lib.Jump as J                    ( Jump(..) )

type Binary = T.Text

encode :: Command -> Binary
encode (Cmd.A a)
    = T.concat ["0", binAddr a]
encode (Cmd.C d c j)
    = T.concat ["111", binComp c, binDest d, binJump j]

binAddr :: Addr -> Binary
binAddr a = T.pack . fill0 15 $ showIntAtBase 2 intToDigit a ""

fill0 :: Int -> String -> String
fill0 width = reverse . take width . (\str -> str ++ repeat '0') . reverse

binComp :: Comp -> Binary
binComp c = case c of
    O       -> "0101010"
    I       -> "0111111"
    DifOI   -> "0111010"
    C.D     -> "0001100"
    C.A     -> "0110000"
    NotD    -> "0001101"
    NotA    -> "0110001"
    DifOD   -> "0001111"
    DifOA   -> "0110011"
    AddDI   -> "0011111"
    AddAI   -> "0110111"
    DifDI   -> "0001110"
    DifAI   -> "0110010"
    AddDA   -> "0000010"
    DifDA   -> "0010011"
    DifAD   -> "0000111"
    AndDA   -> "0000000"
    OrDA    -> "0010101"
    C.M     -> "1110000"
    NotM    -> "1110001"
    DifOM   -> "1110011"
    AddMI   -> "1110111"
    DifMI   -> "1110010"
    AddDM   -> "1000010"
    DifDM   -> "1010011"
    DifMD   -> "1000111"
    AndDM   -> "1000000"
    OrDM    -> "1010101"

binDest :: Dest -> Binary
binDest d = case d of
    D.Null  -> "000"
    D.M     -> "001"
    D.D     -> "010"
    MD      -> "011"
    D.A     -> "100"
    AM      -> "101"
    AD      -> "110"
    AMD     -> "111"

binJump :: Jump -> Binary
binJump j = case j of
    J.Null  -> "000"
    JGT     -> "001"
    JEQ     -> "010"
    JGE     -> "011"
    JLT     -> "100"
    JNE     -> "101"
    JLE     -> "110"
    JMP     -> "111"
