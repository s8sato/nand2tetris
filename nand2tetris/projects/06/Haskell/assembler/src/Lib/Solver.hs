module Lib.Solver where

import qualified Data.HashMap as Map
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

solve :: Solver -> Command -> (Solver, Command)
solve (Solver t p) (Cmd.V sym) = case Map.lookup sym t of
    Just a -> (Solver t p, Cmd.A a)
    Nothing -> (Solver (Map.insert sym p t) (p+1), Cmd.A p)
solve so c = (so, c)
