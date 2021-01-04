{-# LANGUAGE FlexibleContexts #-}

module Main where

import Compiler.Backend.Backend (cumpile)
import Compiler.Backend.Tree (transProgram)
import Compiler.Frontend.Frontend (runSRE)
import Control.Monad (when)
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

run :: Args -> IO ()
run a = do
  fileContent <- readFile $ inp a
  case pProgram (myLexer fileContent) of
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
          let outname = case output a of
                (Just x) -> x
                Nothing -> dropExtension $ inp a
          let base = takeBaseName outname
          let dots = (if asm a then outname else workdir ++ base) ++ ".s"
          let doto = workdir ++ base ++ ".o"
          let libo = workdir ++ "lib.o"
          callCommand $ "mkdir -p " ++ workdir
          writeFile dots $ unlines $ cumpile $ transProgram tree
          callCommand $ "gcc -g -m32 -o " ++ libo ++ " -c " ++ library a
          callCommand $ "gcc -g -m32 -o " ++ doto ++ " -c " ++ dots
          callCommand $ "gcc -g -m32 -o " ++ outname ++ " " ++ doto ++ " " ++ libo
          hPutStrLn stderr "Ok"
          exitSuccess
  where
    workdir = ".latcwork/"

data Args = Args
  { output :: Maybe String,
    library :: String,
    asm :: Bool,
    inp :: String
  }
  deriving (Show)

args :: Parser Args
args =
  Args
    <$> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> help "Output path"
          )
      )
    <*> strOption
      ( long "library"
          <> short 'l'
          <> value "./stdlib/runtime.c"
      )
    <*> switch
      ( long "asm"
          <> short 'a'
          <> help "Whether to emmit asm file before compiling"
      )
    <*> strArgument (help "Latte program to compile")

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> header "latc - Latte compiler for x86 arch"
        )
