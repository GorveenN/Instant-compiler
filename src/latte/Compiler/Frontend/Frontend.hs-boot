module Compiler.Frontend.Frontend where

import AbsLatte as A
import qualified Compiler.Backend.Tree as B
import qualified Compiler.Frontend.Types as T

checkExpr :: Show a => A.Expr a -> T.SRE a B.Type
