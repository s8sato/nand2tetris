{-# LANGUAGE OverloadedStrings #-}

module SymbolTable ( SymbolTable, new ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Text as T

import Lib.Symbol ( Symbol(..) )
import Lib.Addr ( Addr(..) )

type SymbolTable = Map Symbol Addr

new :: SymbolTable
new = Map.fromList
    $ map (\(s, a) -> (Symbol s, Addr a))
    $ ("SP"    , 0     )
    : ("LCL"   , 1     )
    : ("ARG"   , 2     )
    : ("THIS"  , 3     )
    : ("THAT"  , 4     )
    : ("SCREEN", 0x4000)
    : ("KBD"   , 0x6000)
    : map (\x -> (T.pack $ 'R' : show x, x)) [0..15]
