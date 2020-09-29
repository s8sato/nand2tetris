{-# LANGUAGE LambdaCase #-}

module Lib.Labeler where

import qualified Data.HashTable.ST.Basic as HT
import Control.Exception.Safe ( throwString, MonadThrow )
import Control.Monad.ST ( ST )

import SymbolTable ( SymbolTable, new )
import Lib.Label ( Label(..) )
import Lib.Addr as Addr ( check, Addr(..) )

data Labeler s = Labeler {
      nextRomAddr   :: Addr
    , table         :: SymbolTable s
}

new :: ST s (Labeler s)
new = return <$> Labeler (Addr 0) =<< SymbolTable.new

check :: MonadThrow m => Labeler s -> m ()
check lb = case Addr.check (nextRomAddr lb) :: Maybe () of
    Nothing -> throwString $ "Program too large"
    _ -> return ()

inc :: Labeler s -> Labeler s
inc (Labeler p t) = Labeler (p+1) t

insert :: Labeler s -> Label -> ST s ()
insert (Labeler p t) (Label sym) = HT.lookup t sym >>= \case
    Just _ -> throwString $ "Duplicate definition of " ++ show sym
    Nothing -> HT.insert t sym p
