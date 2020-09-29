{-# LANGUAGE LambdaCase #-}

module Lib.Solver where

import qualified Data.HashTable.ST.Basic as HT
import Control.Exception.Safe ( throwString, MonadThrow )
import Control.Monad.ST ( ST )

import SymbolTable ( SymbolTable )
import Lib.Symbol ( Symbol )
import Lib.Addr ( Addr(..) ) 

data Solver s = Solver {
      nextRamAddr   :: Addr
    , table         :: SymbolTable s
}

new :: SymbolTable s -> Solver s
new = Solver $ Addr 0x0010

check :: MonadThrow m => Solver s -> m ()
check so = if 0x4000 < nextRamAddr so
    then throwString "Too many variables"
    else return ()

inc :: Solver s -> Solver s
inc (Solver p t) = Solver (p+1) t

insert :: Solver s -> Symbol -> ST s ()
insert (Solver p t) sym = HT.insert t sym p
