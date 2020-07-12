module Assembler
    ( assemble
    ) where

import Data.List                    ( intercalate )
import Data.Either                  ( isRight
                                    , isLeft
                                    , rights
                                    )
import qualified Data.Text as T

import Parser                       ( parseLines )
import Encoder                      ( Binary
                                    , encode
                                    )
import SymbolTable                  ( defaultTable
                                    , registerLabel
                                    , symbolSolve
                                    )
import Lib.Command as Cmd
import Lib.Addr
import Lib.Util

assemble :: T.Text -> Either String Binary
assemble t
    | all isRight parsed
        = T.unlines
        . map encode
        . symbolSolve table
        . exceptLabel
        <$> addressedCommandsOr
    | otherwise
        = Left $ "invalid syntax in line "
            ++ (intercalate ", " . map show $ leftIndices)
    where
        parsed = map snd $ parseLines t
        leftIndices = map fst . filter (\(_,x) -> isLeft x) $ parseLines t
        addressedCommandsOr = makeAddredCmds $ rights parsed
        table = registerLabel defaultTable $ unRight addressedCommandsOr

exceptLabel :: [(Addr, Command)] -> [Command]
exceptLabel = filter (\c -> case c of
    Cmd.L _ -> False
    _       -> True     ) . map snd

makeAddredCmds :: [Command] -> Either String [(Addr, Command)]
makeAddredCmds cs
    | all isRight $ map fst a'cs
        = Right $ mapFst unRight a'cs
    | otherwise
        = Left "program too large"
    where
        a'cs = mapFst makeAddr i'cs
        i'cs = makeAddredCmds' (zip [0..] cs) []

makeAddredCmds' :: [(Int, Command)] -> [(Int, Command)]  -> [(Int, Command)]
makeAddredCmds' [] out = reverse out
makeAddredCmds' (ic:ics) out = case ic of
    (_, Cmd.L _) -> makeAddredCmds' (mapFst pred ics) (ic:out)
    _            -> makeAddredCmds' ics (ic:out)
