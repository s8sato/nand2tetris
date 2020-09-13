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

import Lib.Command as Cmd               ( Command(V, L, A) )
import Lib.Symbol                       ( Symbol, makeSymbol )
import Lib.Addr                         ( Addr, makeAddr )
import Lib.Util                         ( unRight )

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
symbolSolve st cs = symbolSolve' st cs 0x0010 []

symbolSolve' :: SymbolTable -> [Command] -> Addr -> [Command] -> [Command]
symbolSolve' _ [] _ out = reverse out
symbolSolve' st (c:cs) p out = case c of
    Cmd.V s -> solve st s p cs out
    c       -> symbolSolve' st cs p (c:out)
    where
        solve st s p cs out = case Map.lookup s st of
            Just a -> symbolSolve' st cs p ((Cmd.A a):out)
            _      -> symbolSolve' (Map.insert s p st) cs (p+1) ((Cmd.A p):out)
