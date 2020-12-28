{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.BoundCheck (BoundCheck) where

newtype BoundCheck a = BC { unBC :: a }
    deriving (Show,Eq,Ord,Real,Enum,Integral,Bounded)

mkBC :: (Bounded a, Ord a, Show a) => a -> BoundCheck a
mkBC x | minBound <= x && x <= maxBound = BC x
       | otherwise = error $ "BoundCheck " ++ show x ++ ": out of range"

instance (Bounded a, Ord a, Show a, Num a) => Num (BoundCheck a) where
    BC x + BC y = mkBC (x + y)
    BC x - BC y = mkBC (x - y)
    BC x * BC y = mkBC (x * y)
    negate (BC x) = mkBC (negate x)
    abs (BC x) = mkBC (abs x)
    signum (BC x) = mkBC (signum x)
    fromInteger = mkBC . fromInteger
