module Main where

import System.Environment           ( getArgs )
import System.Directory             ( listDirectory )
import System.FilePath.Windows      ( (-<.>)
                                    , (</>)
                                    , FilePath
                                    , isExtensionOf
                                    , hasExtension
                                    , takeBaseName
                                    )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import VMTranslator                 ( vmTranslate )

main :: IO ()
main = do
    arg <- head <$> getArgs
    let outFile = arg -<.> "asm"
    inFiles <- if hasExtension arg
        then pure [arg]
        else map (arg </>) . filter (isExtensionOf "vm") <$> listDirectory arg
    TIO.writeFile outFile T.empty
    sequence_ . map (inFileWise outFile) $ inFiles
    putStrLn ("Success: " ++ outFile)

inFileWise :: FilePath -> FilePath -> IO ()
inFileWise outFile inFile = do
    input <- TIO.readFile inFile
    let baseName = takeBaseName inFile
    case vmTranslate baseName input of
        Right output -> do
            TIO.appendFile outFile output
            putStrLn ("Partial: " ++ inFile)
        Left err ->
            fail ("Failure: " ++ err ++ " of " ++ inFile)
