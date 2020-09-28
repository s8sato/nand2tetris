{-# LANGUAGE OverloadedStrings #-}

module Assembler ( run ) where

import Data.Char ( isSpace )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception.Safe ( MonadThrow )
import System.IO ( hClose, openFile, IOMode(WriteMode) )
import Control.Monad ( forM_ )
import Control.Monad.ST ( runST )
import Data.STRef ( modifySTRef', newSTRef, readSTRef )

import Encoder ( encode )
import SymbolTable ( SymbolTable )
import Lib.Command ( Command )
import Lib.Config ( Config(inFile, outFile) )
import Lib.Line as Line
    ( Line(Line, body), parse1, parse2, mark, isMarked )
import Lib.Labeler as Labeler
    ( check, inc, insert, new, Labeler(table), lookup )
import Lib.Solver as Solver ( new, check, solve )

import Data.Time ( diffUTCTime, getCurrentTime )

run :: Config -> IO ()
run cfg = do
    t0 <- getCurrentTime
    input <- TIO.readFile $ inFile cfg
    let ls = extract input
    t1 <- getCurrentTime
    putStr "input: "
    print $ diffUTCTime t1 t0

    -- 1st pass
    t0 <- getCurrentTime
    (st, ls') <- pass1 ls
    t1 <- getCurrentTime
    putStr "pass1: "
    print $ diffUTCTime t1 t0

    -- 2nd pass
    t0 <- getCurrentTime
    cs <- pass2 st ls'
    t1 <- getCurrentTime
    putStr "pass2: "
    print $ diffUTCTime t1 t0

    t0 <- getCurrentTime
    writer <- openFile (outFile cfg) WriteMode
    forM_ cs $ \c -> do
        TIO.hPutStrLn writer $ Encoder.encode c
    hClose writer
    t1 <- getCurrentTime
    putStr "write: "
    print $ diffUTCTime t1 t0

extract :: T.Text -> [Line]
extract = filter (not . T.null . body)
        . map (\(i, t) -> Line i t)
        . zip [1..]
        . map (fst . T.breakOn "//" . T.filter (not . isSpace))
        . T.lines

pass1 :: MonadThrow m => [Line] -> m (SymbolTable, [Line])
pass1 ls = return $ runST $ do
    _lb <- newSTRef Labeler.new
    loop _lb ls []
    where
        loop _lb [] out = do
            lb <- readSTRef _lb
            return (Labeler.table lb, filter (not . Line.isMarked) (reverse out))
        loop _lb (l:ls) out = case Line.parse1 l of
            Left _ -> do
                Labeler.check =<< readSTRef _lb
                modifySTRef' _lb Labeler.inc
                loop _lb ls (l:out)
            Right label -> do
                Labeler.lookup label =<< readSTRef _lb
                modifySTRef' _lb $ Labeler.insert label
                loop _lb ls ((Line.mark l):out)

pass2 :: MonadThrow m => SymbolTable -> [Line] -> m [Command]
pass2 st ls = return $ runST $ do
    _so <- newSTRef $ Solver.new st
    loop _so ls
    where
        loop _so [] = do
            Solver.check =<< readSTRef _so
            return []
        loop _so (l:ls) = do
            c  <- Line.parse2 l
            c' <- pure . fst . Solver.solve c =<< readSTRef _so
            modifySTRef' _so $ snd . Solver.solve c
            (:) <$> pure c' <*> loop _so ls
