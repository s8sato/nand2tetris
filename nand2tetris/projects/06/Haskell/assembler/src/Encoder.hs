{-# LANGUAGE BinaryLiterals #-}

module Encoder ( encode ) where

import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )
import Data.Bits ( Bits(shiftL) )
import qualified Data.Text as T

import Lib.Command as Cmd ( Command(C, A) )
import Lib.Addr as Addr ( Addr(..) )
import Lib.Dest as D ( Dest(..) )
import Lib.Comp as C ( Comp(..), IR(..), R(..), AM(..) )
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
            C.O                     -> 0b0101010 -- "0"  
            C.Id C.I                -> 0b0111111 -- "1"  
            C.Neg C.I               -> 0b0111010 -- "-1" 
            C.Id (C.R C.D)          -> 0b0001100 -- "D"  
            C.Id (C.R (C.AM C.A))   -> 0b0110000 -- "A"  
            C.Not C.D               -> 0b0001101 -- "!D" 
            C.Not (C.AM C.A)        -> 0b0110001 -- "!A" 
            C.Neg (C.R C.D)         -> 0b0001111 -- "-D" 
            C.Neg (C.R (C.AM C.A))  -> 0b0110011 -- "-A" 
            C.Inc C.D               -> 0b0011111 -- "D+1"
            C.Inc (C.AM C.A)        -> 0b0110111 -- "A+1"
            C.Dec C.D               -> 0b0001110 -- "D-1"
            C.Dec (C.AM C.A)        -> 0b0110010 -- "A-1"
            C.AddD C.A              -> 0b0000010 -- "D+A"
            C.SubD C.A              -> 0b0010011 -- "D-A"
            C.SubXD C.A             -> 0b0000111 -- "A-D"
            C.AndD C.A              -> 0b0000000 -- "D&A"
            C.OrD C.A               -> 0b0010101 -- "D|A"
            C.Id (C.R (C.AM C.M))   -> 0b1110000 -- "M"  
            C.Not (C.AM C.M)        -> 0b1110001 -- "!M" 
            C.Neg (C.R (C.AM C.M))  -> 0b1110011 -- "-M" 
            C.Inc (C.AM C.M)        -> 0b1110111 -- "M+1"
            C.Dec (C.AM C.M)        -> 0b1110010 -- "M-1"
            C.AddD C.M              -> 0b1000010 -- "D+M"
            C.SubD C.M              -> 0b1010011 -- "D-M"
            C.SubXD C.M             -> 0b1000111 -- "M-D"
            C.AndD C.M              -> 0b1000000 -- "D&M"
            C.OrD C.M               -> 0b1010101 -- "D|M"
        d' = case d of
            Nothing   -> 0b000
            Just d -> case d of
                D.M   -> 0b001
                D.D   -> 0b010
                D.MD  -> 0b011
                D.A   -> 0b100
                D.AM  -> 0b101
                D.AD  -> 0b110
                D.AMD -> 0b111
        j' = case j of
            Nothing   -> 0b000
            Just j -> case j of
                J.JGT -> 0b001
                J.JEQ -> 0b010
                J.JGE -> 0b011
                J.JLT -> 0b100
                J.JNE -> 0b101
                J.JLE -> 0b110
                J.JMP -> 0b111
encode' _ = error "invalid command encode"
