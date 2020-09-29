{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Symbol where

import Data.Char ( isLetter, isDigit )
import Data.Hashable ( Hashable )
import qualified Data.Text as T
import Control.Exception.Safe ( throwString, MonadThrow )

newtype Symbol = Symbol T.Text
    deriving (Show,Eq,Ord,Hashable)

new :: MonadFail m => T.Text -> m Symbol
new x = if isHead (T.head x) && T.all isTail (T.tail x)
    then return $ Symbol x
    else fail "invalid character for symbol"
    where
        isHead c = isLetter c || c `elem` "$:._"
        isTail c = isHead c || isDigit c
