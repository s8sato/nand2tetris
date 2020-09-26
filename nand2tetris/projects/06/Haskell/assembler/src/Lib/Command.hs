module Lib.Command where

import Lib.Symbol ( Symbol )
import Lib.Addr ( Addr )
import Lib.Dest ( Dest )
import Lib.Comp ( Comp )
import Lib.Jump ( Jump )

data Command
    = V Symbol
    | A Addr
    | C (Maybe Dest) Comp (Maybe Jump)
    deriving Show
