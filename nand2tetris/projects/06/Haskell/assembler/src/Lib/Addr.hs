module Lib.Addr
    ( Addr
    , makeAddr
    ) where

import Lib.Natural15

type Addr = Natural15

makeAddr :: (Integral a) => a -> Either String Addr
makeAddr x
    | min_ <= x' && x' <= max_ = Right (fromInteger x')
    | otherwise = Left ""
    where
        x'   = fromIntegral x
        min_ = toInteger (minBound :: Addr)
        max_ = toInteger (maxBound :: Addr)
