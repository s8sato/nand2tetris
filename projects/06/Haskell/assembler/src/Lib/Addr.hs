{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Addr where

newtype Addr = Addr Int
    deriving (Show,Eq,Ord,Num)

new :: (MonadFail m, Show a, Integral a) => a -> m Addr
new x = do
    check x
    return $ Addr $ fromIntegral x

check :: (Ord a, Num a, MonadFail m, Show a) => a -> m ()
check x = if 0 <= x && x < 0x8000
    then return ()
    else fail $ "Out of address range: " ++ show x
