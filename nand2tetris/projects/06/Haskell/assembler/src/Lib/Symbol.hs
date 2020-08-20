{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Symbol
    ( Symbol
    , makeSymbol
    ) where

import Data.Char                        ( isDigit
                                        , isLetter
                                        )
import qualified Data.Text as T

newtype Symbol = S T.Text
    deriving (Show,Eq,Ord)

makeSymbol :: T.Text -> Either String Symbol
makeSymbol x
    | isHead (T.head x) && T.all isTail (T.tail x) = Right (S x)
    | otherwise = Left ""
    where
        isHead c = isLetter c || c `elem` "$:._"
        isTail c = isHead c || isDigit c
