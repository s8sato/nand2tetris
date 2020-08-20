{-# LANGUAGE OverloadedStrings #-}

module SymbolTable
    ( defaultTable
    , registerLabel
    , symbolSolve
    ) where

import Data.Map                         ( Map )
import qualified Data.Map as Map
import Data.List                        ( (\\) )
import Data.Bifunctor                   ( bimap )
import qualified Data.Text as T

import Lib.Command as Cmd
import Lib.Symbol
import Lib.Addr
import Lib.Util

type SymbolTable = Map Symbol Addr

defaultTable :: SymbolTable
defaultTable = Map.fromList
    $ bimap (unRight . makeSymbol) (unRight . makeAddr)
  <$> ("SP"    , 0     )
    : ("LCL"   , 1     )
    : ("ARG"   , 2     )
    : ("THIS"  , 3     )
    : ("THAT"  , 4     )
    : ("SCREEN", 0x4000)
    : ("KBD"   , 0x6000)
    : map (\x -> (T.pack $ 'R' : show x, x)) [0..15]

registerLabel :: SymbolTable -> [(Addr, Command)] -> SymbolTable
registerLabel st [] = st
registerLabel st ((a,(Cmd.L s)):acs) = registerLabel (Map.insert s a st) acs
registerLabel st (_:acs) = registerLabel st acs

symbolSolve :: SymbolTable -> [Command] -> [Command]
symbolSolve st cs = symbolSolve' st cs []

symbolSolve' :: SymbolTable -> [Command] -> [Command] -> [Command]
symbolSolve' st [] out = reverse out
symbolSolve' st (c:cs) out = case c of
    Cmd.V s -> solve st s cs out
    c       -> symbolSolve' st cs (c:out)
    where
        solve st s cs out = case Map.lookup s st of
            Just a -> symbolSolve' st cs ((Cmd.A a):out)
            _      -> symbolSolve' (Map.insert s (newAddr st) st) cs ((Cmd.A $ newAddr st):out)
        newAddr st = head $ ([0..] :: [Addr]) \\ (Map.elems st)
