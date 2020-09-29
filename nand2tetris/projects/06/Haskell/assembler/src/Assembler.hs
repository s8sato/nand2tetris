{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Assembler ( run ) where

import Data.Char ( isSpace )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashTable.ST.Basic as HT
import Control.Exception.Safe ( MonadThrow )
import Control.Monad.ST ( stToIO, ST )
import Data.Time ( diffUTCTime, getCurrentTime )

import Encoder ( encode )
import SymbolTable ( SymbolTable )
import Lib.Command as Cmd ( Command(A, V) )
import Lib.Config ( Config(inFile, outFile) )
import Lib.Line as Line
    ( Line(Line, body), parse1, parse2, mark, isMarked )
import Lib.Labeler as Labeler
    ( Labeler(table), new, check, inc, insert )
import Lib.Solver as Solver
    ( check, new, Solver(table, nextRamAddr), inc, insert )

run :: Config -> IO ()
run cfg = do
    ls          <- extract <$> TIO.readFile (inFile cfg)
    (st, ls')   <- stToIO $ pass1 ls
    cs          <- stToIO $ pass2 st ls'
    TIO.writeFile (outFile cfg) $ T.unlines $ map Encoder.encode cs

extract :: T.Text -> [Line]
extract = filter (not . T.null . body)
        . map (\(i, t) -> Line i t)
        . zip [1..]
        . map (fst . T.breakOn "//" . T.filter (not . isSpace))
        . T.lines

pass1 :: [Line] -> ST s (SymbolTable s, [Line])
pass1 ls = loop ls [] =<< Labeler.new
    where
        loop [] out lb = return
            (Labeler.table lb, filter (not . Line.isMarked) (reverse out))
        loop (l:ls) out lb = case Line.parse1 l of
            Left _ -> do
                Labeler.check lb
                loop ls (l:out) (Labeler.inc lb)
            Right label -> do
                Labeler.insert lb label
                loop ls ((Line.mark l):out) lb

pass2 :: SymbolTable s -> [Line] -> ST s [Command]
pass2 st ls = loop ls $ Solver.new st
    where
        loop [] so = do
            Solver.check so
            return []
        loop (l:ls) so = Line.parse2 l >>= \case
            Cmd.V sym -> HT.lookup (Solver.table so) sym >>= \case
                Just a -> (:) <$> pure (Cmd.A a) <*> loop ls so
                Nothing -> do
                    Solver.insert so sym
                    let c = Cmd.A (nextRamAddr so)
                    (:) <$> pure c <*> loop ls (Solver.inc so)
            c -> (:) <$> pure c <*> loop ls so

time :: String -> IO a -> IO a
time s m = do
    t0 <- getCurrentTime
    res <- m
    t1 <- getCurrentTime
    putStr $ s ++ ": "
    print $ diffUTCTime t1 t0
    return res
