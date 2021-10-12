module Lib.Command where

import Lib.Addr
import Lib.Arithmetic
import Lib.Segment
import Lib.Symbol

data Command
    = Arithmetic Arithmetic
    | Push Segment Addr
    | Pop Segment Addr
    | Label Symbol
    | Goto Symbol
    | If Symbol
    | Function Symbol Addr
    | Call Symbol Addr
    | Return
    deriving Show
