module Lib.Line where

import Data.Text ( Text )
import Data.Attoparsec.Text ( parseOnly )
import Control.Exception.Safe ( throwString, MonadThrow )

import Parser ( aCommand, aLabel )
import Lib.Label ( Label )
import Lib.Command ( Command )

data Line = Line {
      index  :: Int
    , body   :: Text
}

parse1 :: Line -> Either String Label
parse1 = parseOnly aLabel <$> body

parse2 :: MonadThrow m => Line -> m Command
parse2 l = case parseOnly aCommand $ body l of
    Left _ -> throwString $ "line " ++ show (index l) ++ ", parse failed"
    Right cmd -> return cmd

mark :: Line -> Line
mark (Line i b) = Line 0 b

isMarked :: Line -> Bool
isMarked (Line i _) = i == 0
