module Lib.Labeler where

import qualified Data.HashMap as Map
import Control.Exception.Safe ( throwString, MonadThrow )

import SymbolTable ( SymbolTable, new )
import Lib.Label ( Label(..) )
import Lib.Addr as Addr ( Addr(..), check )

data Labeler = Labeler {
      table         :: SymbolTable
    , nextRomAddr   :: Addr
}

new :: Labeler
new = Labeler {
      table         = SymbolTable.new
    , nextRomAddr   = Addr 0
}

check :: MonadThrow m => Labeler -> m ()
check lb = case Addr.check (nextRomAddr lb) :: Maybe () of
    Nothing -> throwString $ "Program too large"
    _ -> return ()

inc :: Labeler -> Labeler
inc (Labeler t p) = Labeler t (p+1)

insert :: MonadThrow m => Labeler -> Label -> m Labeler
insert (Labeler t p) (Label sym) = case Map.lookup sym t of
    Just _ -> throwString $ "Duplicate definition of " ++ show sym
    Nothing -> return $ Labeler (Map.insert sym p t) p
