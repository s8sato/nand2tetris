module Main where

import System.Environment           ( getArgs )
import System.FilePath.Windows      ( replaceExtension )
import Data.List                    ( intercalate )
import Data.Either                  ( isRight
                                    , isLeft
                                    , rights
                                    )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser                       ( parseLines )
import Code                         ( Binary
                                    , encode
                                    )

main :: IO ()
main = do
    args <- getArgs
    let inFile = head args
    let outFile = replaceExtension inFile "hack"
    input <- TIO.readFile inFile
    case assemble input of
        Right output -> do
            TIO.writeFile outFile output
            putStrLn ("Success: " ++ outFile)
        Left err -> do
            putStrLn ("Failure: invalid syntax in line " ++ err)

assemble :: T.Text -> Either String Binary
assemble t
    | all isRight parsed
        = Right . T.unlines . map encode . rights $ parsed
    | otherwise
        = Left . intercalate ", " . map show $ leftIndices
    where
        parsed      = map snd $ parseLines t
        leftIndices = map fst . filter (\(_, x) -> isLeft x) $ parseLines t
