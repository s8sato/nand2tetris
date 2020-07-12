module VMTranslator
    ( vmTranslate
    ) where

import Data.List                    ( intercalate )
import Data.Either                  ( isRight
                                    , isLeft
                                    , rights
                                    )
import qualified Data.Text as T

import Parser                       ( parseLines )
import Encoder                      ( encode
                                    , BaseName
                                    )

vmTranslate :: BaseName -> T.Text -> Either String T.Text
vmTranslate f t
    | all isRight parsed
        = Right . T.unlines . encode f . rights $ parsed
    | otherwise
        = Left $ "invalid syntax in line "
            ++ (intercalate ", " . map show $ leftIndices)
    where
        parsed = map snd $ parseLines t
        leftIndices = map fst . filter (\(_,x) -> isLeft x) $ parseLines t
