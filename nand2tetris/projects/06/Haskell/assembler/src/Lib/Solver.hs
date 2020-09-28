module Lib.Solver where

import qualified Data.Map as Map
import Control.Exception.Safe ( throwString, MonadThrow )

import SymbolTable ( SymbolTable )
import Lib.Command as Cmd ( Command(A, V) )
import Lib.Addr ( Addr(..) )

data Solver = Solver {
      table         :: SymbolTable
    , nextRamAddr   :: Addr
}

new :: SymbolTable -> Solver
new st = Solver {
      table         = st
    , nextRamAddr   = Addr 0x0010
}

check :: MonadThrow m => Solver -> m ()
check (Solver _ p) = if 0x4000 < p
    then throwString "Too many variables"
    else return ()

solve :: Command -> Solver -> (Command, Solver)
solve (Cmd.V sym) (Solver t p)  = case Map.lookup sym t of
    Just a -> (Cmd.A a, Solver t p) 
    Nothing -> (Cmd.A p, Solver (Map.insert sym p t) (p+1))
solve c so = (c, so)
