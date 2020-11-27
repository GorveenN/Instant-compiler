{-# LANGUAGE FlexibleContexts #-}

module Main where

import Compiler.Frontend.Frontend (runSRE)
import Control.Monad (when)
import ErrM
import LexLatte
import ParLatte
import PrintLatte
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Text.Pretty.Simple (pPrint)

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

-- runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run v p s =
  case p (myLexer s) of
    Bad s -> do
      hPutStrLn stderr $ "Parse Failed\n" ++ s
      exitFailure
    Ok tree -> do
      putStrV v $ printTree tree
      let res = runSRE tree
      case res of
        (Left exc) -> do
          hPutStrLn stderr ("Static check failed\n" ++ show exc)
          exitFailure
        (Right ()) -> do
          putStrLn "Parse Successful"
          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ pPrint tree
  pPrint tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Parse stdin verbosely.",
        "  (files)         Parse content of files verbosely.",
        "  -s (files)      Silent mode. Parse content of files silently."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s" : fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs
