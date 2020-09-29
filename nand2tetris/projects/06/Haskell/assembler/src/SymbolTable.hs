{-# LANGUAGE OverloadedStrings #-}

module SymbolTable ( SymbolTable, new ) where

import qualified Data.Text as T
import qualified Data.HashTable.ST.Basic as HT
import Data.HashTable.Class ( fromList )
import Control.Monad.ST ( ST )

import Lib.Symbol ( Symbol(..) )
import Lib.Addr ( Addr(..) )

type SymbolTable s = HT.HashTable s Symbol Addr

new :: ST s (SymbolTable s)
new = fromList
    $ map (\(s, a) -> (Symbol s, Addr a))
    $ ("SP"    , 0     )
    : ("LCL"   , 1     )
    : ("ARG"   , 2     )
    : ("THIS"  , 3     )
    : ("THAT"  , 4     )
    : ("SCREEN", 0x4000)
    : ("KBD"   , 0x6000)
    : map (\x -> (T.pack $ 'R' : show x, x)) [0..15]
