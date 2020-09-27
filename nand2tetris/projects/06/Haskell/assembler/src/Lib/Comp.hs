module Lib.Comp where

data Comp
    = O
    | Id IR
    | Neg IR
    | Not R
    | Inc R
    | Dec R
    | AddD AM
    | SubD AM
    | SubXD AM
    | AndD AM
    | OrD AM
    deriving Show

data IR
    = I
    | R R
    deriving Show

data R
    = D
    | AM AM
    deriving Show

data AM
    = A
    | M
    deriving Show
