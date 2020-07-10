module Main where

import System.Environment           ( getArgs )
import System.FilePath.Windows      ( replaceExtension )
import qualified Data.Text.IO as TIO

import Assembler                    ( assemble )

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
            putStrLn ("Failure: " ++ err)
