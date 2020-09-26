module Lib.Config where

import Control.Monad ( when )
import Control.Exception.Safe ( throwString, MonadThrow )
import System.FilePath.Posix ( replaceExtension )

data Config = Config {
      inFile  :: FilePath
    , outFile :: FilePath
}

new :: MonadThrow m => [String] -> m Config
new args = do
    when (length args /= 1) $
        throwString "Specify exactly one argument"
    let inF = head args
    let outF = replaceExtension inF "hack"
    return Config { inFile = inF, outFile = outF }
