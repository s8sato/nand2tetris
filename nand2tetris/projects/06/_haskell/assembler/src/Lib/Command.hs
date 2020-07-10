module Lib.Command where

import Lib.Symbol
import Lib.Natural15
import Lib.Comp
import Lib.Dest
import Lib.Jump

data Command
    = L Symbol
    | V Symbol
    | A Natural15
    | C Dest Comp Jump
    deriving Show
