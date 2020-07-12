module Lib.Util where

mapFst f = map (\(a,b) -> (f a, b))
mapSnd f = map (\(a,b) -> (a, f b))
unRight (Right x) = x
