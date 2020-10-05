{-# LANGUAGE OverloadedStrings #-}

module Assembler ( run ) where

import Data.Char ( isSpace )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception.Safe ( MonadThrow )
import Data.Time ( diffUTCTime, getCurrentTime )
import Control.DeepSeq ( NFData(..), deepseq )

import Encoder ( encode )
import SymbolTable ( SymbolTable )
import Lib.Symbol ( Symbol )
import Lib.Command ( Command )
import Lib.Addr ( Addr )
import Lib.Config ( Config(inFile, outFile) )
import Lib.Line as Line
    ( Line(Line, body), parse1, parse2, mark, isMarked )
import Lib.Labeler as Labeler
    ( Labeler(table), new, check, inc, insert )
import Lib.Solver as Solver ( new, check, solve )

run :: Config -> IO ()
run cfg = do
    ls          <- time "input" $ extract <$> TIO.readFile (inFile cfg)
    (st, ls')   <- time "pass1" $ pass1 ls
    cs          <- time "pass2" $ pass2 st ls'
    time "write" $ TIO.writeFile (outFile cfg) $ T.unlines $ map Encoder.encode cs

extract :: T.Text -> [Line]
extract = filter (not . T.null . body)
        . map (\(i, t) -> Line i t)
        . zip [1..]
        . map (fst . T.breakOn "//" . T.filter (not . isSpace))
        . T.lines

pass1 :: MonadThrow m => [Line] -> m (SymbolTable, [Line])
pass1 ls = loop Labeler.new ls []
    where
        loop lb [] out = return
            (Labeler.table lb, filter (not . Line.isMarked) (reverse out))
        loop lb (l:ls) out = case Line.parse1 l of
            Left _ -> do
                Labeler.check lb
                loop (Labeler.inc lb) ls (l:out)
            Right label -> do
                lb' <- Labeler.insert lb label
                loop lb' ls ((Line.mark l):out)

pass2 :: MonadThrow m => SymbolTable -> [Line] -> m [Command]
pass2 st ls = loop (Solver.new st) ls
    where
        loop so [] = do
            Solver.check so
            return []
        loop so (l:ls) = do
            c <- Line.parse2 l
            let (so', c') = Solver.solve so c
            (:) <$> pure c' <*> loop so' ls

time :: NFData a => String -> IO a -> IO a
time s m = do
    t0 <- getCurrentTime
    res <- m
    t1 <- getCurrentTime
    putStr $ s ++ ": "
    print $ diffUTCTime t1 t0
    return res

instance NFData Symbol where
    rnf _ = ()
instance NFData Command where
    rnf _ = ()
instance NFData Addr where 
    rnf _ = () 
instance NFData Line where
    rnf _ = ()
