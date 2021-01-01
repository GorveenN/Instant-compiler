{-# LANGUAGE FlexibleContexts #-}

module Main where

import Compiler.Backend.Backend (cumpile)
import Compiler.Backend.Tree (transProgram)
import Compiler.Frontend.Frontend (runSRE)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Semigroup ((<>))
import ErrM
import GHC.IO.Handle.FD (stdout)
import LexLatte
import Options.Applicative
import ParLatte
import PrintLatte
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO (hPrint, hPutStrLn, stderr)
import System.Process

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

-- runFile v p f = readFile f >>= run v p
runFile p f = readFile f >>= run p f

run p n s =
  case p (myLexer s) of
    Bad s -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr "Parse Error"
      hPrint stderr $ show s
      exitFailure
    Ok tree -> do
      case runSRE tree of
        (Left exc) -> do
          hPutStrLn stderr "ERROR"
          hPutStrLn stderr ("Static check failed\n" ++ show exc)
          exitFailure
        (Right ()) -> do
          let dots = replaceExtension n "s"
          let doto = replaceExtension n "o"
          let base = dropExtension n
          let prog = cumpile $ transProgram tree
          writeFile dots $ unlines prog
          callCommand "gcc -g -m32 -o build/runtime.o -c build/runtime.c"
          callCommand $ "gcc -g -m32 -o " ++ doto ++ " -c " ++ dots
          callCommand $ "gcc -g -m32 -o " ++ base ++ " " ++ doto ++ " build/runtime.o"
          -- hPutStrLn stderr "Ok"
          exitSuccess

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  mapM_ (runFile pProgram) args

-- data Sample = Sample
--   { output :: String,
--     library :: String,
--     quiet :: Bool,
--     asm :: Bool,
--     arg :: String
--   }
--   deriving (Show)

-- sample :: Parser Sample
-- sample =
--   Sample
--     <$> strOption
--       ( long "output"
--           <> short 'o'
--           <> metavar "TARGET"
--           <> help "Output path"
--       )
--     <*> strOption
--       ( long "library"
--           <> short 'l'
--           <> value "./build/runtime.c"
--           <> help "Whether to be quiet"
--       )
--     <*> switch
--       ( long "quiet"
--           <> short 'q'
--           <> help "Whether to be quiet"
--       )
--     <*> switch
--       ( long "asm"
--           <> short 'a'
--           <> help "Whether to emmit asm file before compiling"
--       )
--     <*> argument str (help "ala ma kota")

-- main :: IO ()
-- main = execParser opts >>= greet
--   where
--     opts =
--       info
--         (sample <**> helper)
--         ( fullDesc
--             <> progDesc "Print a greeting for TARGET"
--             <> header "hello - a test for optparse-applicative"
--         )

-- greet :: Sample -> IO ()
-- greet = print
