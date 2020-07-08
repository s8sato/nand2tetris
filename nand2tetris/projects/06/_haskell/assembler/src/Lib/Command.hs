module Lib.Command where

import Lib.Comp
import Lib.Dest
import Lib.Jump
import Lib.Natural15

data Command
    = C Dest Comp Jump
    | A Natural15
    deriving Show
