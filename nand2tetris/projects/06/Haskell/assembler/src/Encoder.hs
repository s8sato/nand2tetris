{-# LANGUAGE BinaryLiterals #-}

module Encoder ( encode ) where

import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )
import Data.Bits ( Bits(shiftL) )
import qualified Data.Text as T

import Lib.Command as Cmd ( Command(C, A) )
import Lib.Addr as Addr ( Addr(..) )
import Lib.Dest as D ( Dest(..) )
import Lib.Comp as C ( Comp(..) )
import Lib.Jump as J ( Jump(..) )

type Binary = T.Text

encode :: Command -> Binary
encode c = T.pack . fill0 16 $ showIntAtBase 2 intToDigit (encode' c) ""
    where
        fill0 width = reverse . take width . (\s -> s ++ repeat '0') . reverse

encode' :: Command -> Int
encode' (Cmd.A (Addr a)) = a
encode' (Cmd.C d c j) = (0b111 `shiftL` 13) + foldl (\acc x -> (acc `shiftL` 3) + x) 0 [c', d', j']
    where
        c' = case c of
            O       -> 0b0101010
            I       -> 0b0111111
            DifOI   -> 0b0111010
            C.D     -> 0b0001100
            C.A     -> 0b0110000
            NotD    -> 0b0001101
            NotA    -> 0b0110001
            DifOD   -> 0b0001111
            DifOA   -> 0b0110011
            AddDI   -> 0b0011111
            AddAI   -> 0b0110111
            DifDI   -> 0b0001110
            DifAI   -> 0b0110010
            AddDA   -> 0b0000010
            DifDA   -> 0b0010011
            DifAD   -> 0b0000111
            AndDA   -> 0b0000000
            OrDA    -> 0b0010101
            C.M     -> 0b1110000
            NotM    -> 0b1110001
            DifOM   -> 0b1110011
            AddMI   -> 0b1110111
            DifMI   -> 0b1110010
            AddDM   -> 0b1000010
            DifDM   -> 0b1010011
            DifMD   -> 0b1000111
            AndDM   -> 0b1000000
            OrDM    -> 0b1010101
        d' = case d of
            Nothing -> 0b000
            Just d -> case d of
                D.M -> 0b001
                D.D -> 0b010
                MD  -> 0b011
                D.A -> 0b100
                AM  -> 0b101
                AD  -> 0b110
                AMD -> 0b111
        j' = case j of
            Nothing -> 0b000
            Just j -> case j of
                JGT -> 0b001
                JEQ -> 0b010
                JGE -> 0b011
                JLT -> 0b100
                JNE -> 0b101
                JLE -> 0b110
                JMP -> 0b111
encode' _ = error "invalid command encode"
