module Compiler.Backend.Backend where

import Compiler.Backend.CodeGen
import Compiler.Backend.Tree
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer
  ( Writer,
    WriterT,
    runWriter,
    tell,
  )

runCodeGen program = runWriter $ runReaderT (evalStateT program 1) 1

immediateToRegister :: Register -> Operand -> CodeGen Operand
immediateToRegister r c@(Const _) = do
  let reg = Register r Nothing
  mov_ c reg
  return reg
immediateToRegister r o = return o

-- result of Expr is packed in eax register or is a constant
emmitExpr :: Expr -> CodeGen Operand
emmitExpr (ELitInt i) = return $ Const i
emmitExpr ELitTrue = return $ Const 1
emmitExpr ELitFalse = return $ Const 0
emmitExpr ENull = return $ Const 0
emmitExpr (EString s) = undefined
emmitExpr (EApp n args) = undefined
emmitExpr (Neg e) = undefined
emmitExpr (Not e) = undefined
emmitExpr (EArithm e1 op e2) = case op of
  Plus -> addsub add_ e1 e2
  Minus -> addsub sub_ e1 e2
  Times -> addsub imul_ e1 e2
  Div -> do
    basediv e1 e2
    return (Register EAX Nothing)
  Mod -> do
    basediv e1 e2
    mov_ (Register EDX Nothing) (Register EAX Nothing)
    return (Register EAX Nothing)
  where
    addsub ctr e1 e2 = do
      push_ =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToRegister EAX
      let op2 = Register EDX Nothing
      pop_ op2
      ctr op2 op1
      return op2

    basediv e1 e2 = do
      push_ =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToRegister EAX
      let divisor = Register EBX Nothing
      pop_ divisor
      cdq_
      idiv_ divisor
emmitExpr (ELogic e1 op e2) = case op of
  LTH -> undefined
  LE -> undefined
  GTH -> undefined
  GE -> undefined
  EQU -> undefined
  NE -> undefined
  AND -> undefined
  OR -> undefined

emmitStmt :: Stmt -> CodeGen ()
emmitStmt (Block ss) = undefined
emmitStmt (Decl t n e) = undefined
emmitStmt (Ass e1 e2) = undefined
emmitStmt (Incr e) = undefined
emmitStmt (Decr e) = undefined
emmitStmt (Ret e) = undefined
emmitStmt VRet = undefined
emmitStmt (Cond e s) = undefined
emmitStmt (CondElse e s1 s2) = undefined
emmitStmt (While e s) = undefined
emmitStmt (For t n e s) = undefined
emmitStmt (SExp e) = undefined

emmitProgram :: Program -> CodeGen ()
emmitProgram (Program defs) = undefined

emmitTopDef :: TopDef -> CodeGen ()
emmitTopDef (TopClassDef cls) = undefined
emmitTopDef (TopFnDef cls) = undefined

-- push   %ebp
-- mov    %esp,%ebp
-- mov    $0x2a,%eax
-- pop    %ebp
-- ret

-- push   %ebp
-- mov    %esp,%ebp
-- call   8 <_ZN3AlaC2Ev+0x8>
-- pop    %eax
-- add    $0x3,%eax
-- mov    0x8(%ebp),%ecx
-- lea    0x0(%eax),%eax
-- add    $0x8,%eax
-- mov    0x8(%ebp),%edx
-- mov    %eax,(%edx)
-- pop    %ebp
-- ret

-- push   %ebp
-- mov    %esp,%ebp
-- push   %eax
-- mov    0xc(%ebp),%eax
-- mov    0x8(%ebp),%ecx
-- mov    $0x2a,%edx
-- mov    %eax,-0x4(%ebp)
-- mov    %edx,%eax
-- add    $0x4,%esp
-- pop    %ebp
-- ret
