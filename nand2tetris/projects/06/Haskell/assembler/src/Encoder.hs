{-# LANGUAGE BinaryLiterals #-}

module Encoder ( encode ) where

import Numeric ( showIntAtBase )
import Data.Char ( intToDigit )
import qualified Data.Text as T

import Lib.Command as Cmd ( Command(C, A) )
import Lib.Addr as Addr ( Addr(..) )
import Lib.Dest as D ( Dest(..) )
import Lib.Comp as C ( Comp(..), IR(..), R(..), AM(..) )
import Lib.Jump as J ( Jump(..) )

type Binary = T.Text

encode :: Command -> Binary
encode (Cmd.A (Addr a)) = T.pack $ (:) '0' $ fill0 15 $ showIntAtBase 2 intToDigit a ""
    where
        fill0 width = reverse . take width . (\s -> s ++ repeat '0') . reverse
encode (Cmd.C d c j) = T.pack $ c' $ d' $ j'
    where
        c' = case c of
            C.O                     -> (\cs -> '0':'1':'0':'1':'0':'1':'0':cs) -- "0"  
            C.Id C.I                -> (\cs -> '0':'1':'1':'1':'1':'1':'1':cs) -- "1"  
            C.Neg C.I               -> (\cs -> '0':'1':'1':'1':'0':'1':'0':cs) -- "-1" 
            C.Id (C.R C.D)          -> (\cs -> '0':'0':'0':'1':'1':'0':'0':cs) -- "D"  
            C.Id (C.R (C.AM C.A))   -> (\cs -> '0':'1':'1':'0':'0':'0':'0':cs) -- "A"  
            C.Not C.D               -> (\cs -> '0':'0':'0':'1':'1':'0':'1':cs) -- "!D" 
            C.Not (C.AM C.A)        -> (\cs -> '0':'1':'1':'0':'0':'0':'1':cs) -- "!A" 
            C.Neg (C.R C.D)         -> (\cs -> '0':'0':'0':'1':'1':'1':'1':cs) -- "-D" 
            C.Neg (C.R (C.AM C.A))  -> (\cs -> '0':'1':'1':'0':'0':'1':'1':cs) -- "-A" 
            C.Inc C.D               -> (\cs -> '0':'0':'1':'1':'1':'1':'1':cs) -- "D+1"
            C.Inc (C.AM C.A)        -> (\cs -> '0':'1':'1':'0':'1':'1':'1':cs) -- "A+1"
            C.Dec C.D               -> (\cs -> '0':'0':'0':'1':'1':'1':'0':cs) -- "D-1"
            C.Dec (C.AM C.A)        -> (\cs -> '0':'1':'1':'0':'0':'1':'0':cs) -- "A-1"
            C.AddD C.A              -> (\cs -> '0':'0':'0':'0':'0':'1':'0':cs) -- "D+A"
            C.SubD C.A              -> (\cs -> '0':'0':'1':'0':'0':'1':'1':cs) -- "D-A"
            C.SubXD C.A             -> (\cs -> '0':'0':'0':'0':'1':'1':'1':cs) -- "A-D"
            C.AndD C.A              -> (\cs -> '0':'0':'0':'0':'0':'0':'0':cs) -- "D&A"
            C.OrD C.A               -> (\cs -> '0':'0':'1':'0':'1':'0':'1':cs) -- "D|A"
            C.Id (C.R (C.AM C.M))   -> (\cs -> '1':'1':'1':'0':'0':'0':'0':cs) -- "M"  
            C.Not (C.AM C.M)        -> (\cs -> '1':'1':'1':'0':'0':'0':'1':cs) -- "!M" 
            C.Neg (C.R (C.AM C.M))  -> (\cs -> '1':'1':'1':'0':'0':'1':'1':cs) -- "-M" 
            C.Inc (C.AM C.M)        -> (\cs -> '1':'1':'1':'0':'1':'1':'1':cs) -- "M+1"
            C.Dec (C.AM C.M)        -> (\cs -> '1':'1':'1':'0':'0':'1':'0':cs) -- "M-1"
            C.AddD C.M              -> (\cs -> '1':'0':'0':'0':'0':'1':'0':cs) -- "D+M"
            C.SubD C.M              -> (\cs -> '1':'0':'1':'0':'0':'1':'1':cs) -- "D-M"
            C.SubXD C.M             -> (\cs -> '1':'0':'0':'0':'1':'1':'1':cs) -- "M-D"
            C.AndD C.M              -> (\cs -> '1':'0':'0':'0':'0':'0':'0':cs) -- "D&M"
            C.OrD C.M               -> (\cs -> '1':'0':'1':'0':'1':'0':'1':cs) -- "D|M"
        d' = case d of
            Nothing   -> (\cs -> '0':'0':'0':cs)
            Just d -> case d of
                D.M   -> (\cs -> '0':'0':'1':cs)
                D.D   -> (\cs -> '0':'1':'0':cs)
                D.MD  -> (\cs -> '0':'1':'1':cs)
                D.A   -> (\cs -> '1':'0':'0':cs)
                D.AM  -> (\cs -> '1':'0':'1':cs)
                D.AD  -> (\cs -> '1':'1':'0':cs)
                D.AMD -> (\cs -> '1':'1':'1':cs)
        j' = case j of
            Nothing   -> "000"
            Just j -> case j of
                J.JGT -> "001"
                J.JEQ -> "010"
                J.JGE -> "011"
                J.JLT -> "100"
                J.JNE -> "101"
                J.JLE -> "110"
                J.JMP -> "111"
encode _ = error "invalid command encode"
