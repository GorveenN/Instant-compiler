module Compiler.Frontend.Utils where

import           AbsLatte
import           Compiler.Frontend.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map

exprPosition :: Expr a -> a
exprPosition expr = case expr of
    EVar    pos _         -> pos
    ELitInt pos _         -> pos
    ELitTrue  pos         -> pos
    ELitFalse pos         -> pos
    EApp pos _ _          -> pos
    EString pos _         -> pos
    EAccess pos _ _       -> pos
    Neg pos _             -> pos
    Not pos _             -> pos
    EMul pos _ _ _        -> pos
    EAdd pos _ _ _        -> pos
    ERel pos _ _ _        -> pos
    EAnd pos _ _          -> pos
    EOr  pos _ _          -> pos
    ENewObject pos _      -> pos
    ENewArray pos _ _     -> pos
    EMember   pos _ _     -> pos
    EMemberCall pos _ _ _ -> pos
    ENull pos             -> pos
    ECast pos _           -> pos
