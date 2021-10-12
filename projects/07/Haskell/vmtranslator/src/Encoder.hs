{-# LANGUAGE OverloadedStrings #-}

module Encoder
    ( encode
    , BaseName
    ) where

import Numeric                          ( showInt )
import Data.Bifunctor                   ( bimap )
import qualified Data.Text as T

import Lib.Command
-- import Lib.Symbol
import Lib.Addr
import Lib.Arithmetic
import Lib.Segment

type BaseName = String

encode :: BaseName -> [Command] -> [T.Text]
encode f cs = encode' f 0 cs []

encode' :: BaseName -> Addr -> [Command] -> [T.Text] -> [T.Text]
encode' _ _ [] out     = reverse out
encode' f j (c:cs) out = case c of
    Arithmetic  a   ->  let res = fst $ encodeA f j a
                            inc = snd $ encodeA f j a
                        in encode' f (j+inc) cs (res:out)
    Push        s i -> encode' f j cs ((encodePU f s i):out)
    Pop         s i -> encode' f j cs ((encodePO f s i):out)
    Label       s   -> []
    Goto        s   -> []
    If          s   -> []
    Function    s i -> []
    Call        s i -> []
    Return          -> []

encodeA :: BaseName -> Addr -> Arithmetic -> (T.Text, Addr)
encodeA f j a = bimap T.unlines fromInteger (case a of
    Add -> ([preBinaryOp, "M=D+M"]               , 0)
    Sub -> ([preBinaryOp, "M=M-D"]               , 0)
    Neg -> ([preUnaryOp,  "M=-M"]                , 0)
    Eq  -> ([preBinaryOp, comparisonOp f j "JEQ"], 2)
    Gt  -> ([preBinaryOp, comparisonOp f j "JGT"], 2)
    Lt  -> ([preBinaryOp, comparisonOp f j "JLT"], 2)
    And -> ([preBinaryOp, "M=D&M"]               , 0)
    Or  -> ([preBinaryOp, "M=D|M"]               , 0)
    Not -> ([preUnaryOp,  "M=!M"]                , 0)
    )
    where
        preBinaryOp = T.unlines
            [ "@SP"
            , "AM=M-1"
            , "D=M"
            , "A=A-1"
            ]
        preUnaryOp = T.unlines
            [ "@SP"
            , "A=M-1"
            ]
        comparisonOp f j jump = T.unlines
            [ "D=M-D"
            , atVar f j
            , "D;" `T.append` jump
            , "@SP"
            , "A=M-1"
            , "M=0"
            , atVar f (j+1)
            , "0;JMP"
            , labelVar f j
            , "@SP"
            , "A=M-1"
            , "M=-1"
            , labelVar f (j+1)
            ]

encodePU :: BaseName -> Segment -> Addr -> T.Text
encodePU f s i = T.unlines $ (case s of
    Constant -> toD i
    Local    -> toDM "LCL" i
    Argument -> toDM "ARG" i
    This     -> toDM "THIS" i
    That     -> toDM "THAT" i
    Pointer  -> toDA 3 i
    Temp     -> toDA 5 i
    Static   ->
        [ atVar f i
        , "D=M"
        ]
    ) ++ pushD
    where
        toD i =
            [ atConst i
            , "D=A"
            ]
        toDM seg i = toD i ++
            [ atLabel seg
            , "A=D+M"
            , "D=M"
            ]
        toDA a i = toD i ++
            [ atConst a
            , "A=D+A"
            , "D=M"
            ]
        pushD =
            [ "@SP"
            , "A=M"
            , "M=D"
            , "@SP"
            , "M=M+1"
            ]

encodePO :: BaseName -> Segment -> Addr -> T.Text
encodePO f s i = T.unlines (case s of
    Constant -> popD
    Local    -> setRM "LCL" i   ++ popD ++ toR_D
    Argument -> setRM "ARG" i   ++ popD ++ toR_D
    This     -> setRM "THIS" i  ++ popD ++ toR_D
    That     -> setRM "THAT" i  ++ popD ++ toR_D
    Pointer  -> setRA 3 i       ++ popD ++ toR_D
    Temp     -> setRA 5 i       ++ popD ++ toR_D
    Static   -> popD ++
        [ atVar f i
        , "M=D"
        ]
    )
    where
        toD i =
            [ atConst i
            , "D=A"
            ]
        setRM seg i = toD i ++
            [ atLabel seg
            , "D=D+M"
            , "@R13"
            , "M=D"
            ]
        setRA a i = toD i ++
            [ atConst a
            , "D=D+A"
            , "@R13"
            , "M=D"
            ]
        popD =
            [ "@SP"
            , "AM=M-1"
            , "D=M"
            ]
        toR_D =
            [ "@R13"
            , "A=M"
            , "M=D"
            ]

atConst :: Addr -> T.Text
atConst a = T.pack $ '@' : showInt a ""

atVar :: BaseName -> Addr -> T.Text
atVar f j = T.pack $ '@' : f ++ '.' : showInt j ""

atLabel :: T.Text -> T.Text
atLabel l = '@' `T.cons` l

label :: T.Text -> T.Text
label l = T.concat ["(", l, ")"]

labelVar :: BaseName -> Addr -> T.Text
labelVar f j = T.pack $ '(' : f ++ '.' : showInt j ")"
