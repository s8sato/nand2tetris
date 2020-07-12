{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Natural15 (Natural15) where

import Numeric.Natural                  ( Natural )
import Lib.BoundCheck                   ( BoundCheck )

type Natural15 = BoundCheck Natural15'

newtype Natural15' = N15 Natural
    deriving (Show,Eq,Ord,Num,Real,Enum,Integral)

instance Bounded Natural15' where
    minBound = fromInteger 0
    maxBound = fromInteger (2^15-1)
