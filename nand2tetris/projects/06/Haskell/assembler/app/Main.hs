import System.Environment ( getArgs )
import System.IO ( stderr, hPutStrLn )
import Control.Exception.Safe
    ( catch, StringException(StringException) )

import Assembler ( run )
import Lib.Config as Config ( new, Config(outFile) )

main :: IO ()
main = do
    args <- getArgs
    cfg <- Config.new args
    Assembler.run cfg
    putStrLn $ "Success: " ++ outFile cfg
    `catch` \(StringException s _) -> hPutStrLn stderr s
