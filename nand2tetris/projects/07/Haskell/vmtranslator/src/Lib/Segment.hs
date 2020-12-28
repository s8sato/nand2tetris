module Lib.Segment where

data Segment
    = Constant
    | Local
    | Argument
    | This
    | That
    | Pointer
    | Temp
    | Static
    deriving Show
