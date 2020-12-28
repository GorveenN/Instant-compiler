module Compiler.Backend.Backend where

import Compiler.Backend.CodeGen
import Compiler.Backend.Tree
import Control.Lens hiding
  ( Const,
    Empty,
    element,
  )
import Control.Monad
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.Reader
  ( ReaderT,
    asks,
    local,
    runReaderT,
  )
import Control.Monad.State
  ( StateT,
    evalStateT,
    get,
    modify,
  )
import Control.Monad.Writer
  ( Writer,
    WriterT,
    execWriter,
    runWriter,
    tell,
  )
import qualified Data.Map as Map

instance MonadFail Identity where
  fail = error "Fail"

runCodeGen :: Program -> [Instruction]
runCodeGen program =
  execWriter $
    runReaderT
      (evalStateT (emmitProgram program) 1)
      (Store {_vars = Map.empty, _stackH = 0})

immediateToOperand :: Operand -> Operand -> CodeGen Operand
immediateToOperand r c@(Const _) = do
  mov_ c r
  return r
immediateToOperand r o = return o

movIfNescessary :: Operand -> Operand -> CodeGen Operand
movIfNescessary o1 o2 =
  if o1 == o2
    then return o1
    else do
      mov_ o2 o1
      return o1

getVar :: String -> CodeGen Operand
getVar n = do
  i <- asks ((Map.! n) . _vars)
  return $ Memory EBP (Just i)

-- result of Expr is packed in eax register or is a constant
emmitExpr :: Expr -> CodeGen Operand
emmitExpr (ELitInt i) = return $ Const i
emmitExpr (EString s) = undefined
emmitExpr (EVar v) = getVar v
emmitExpr (EApp n args) = do
  mapM_ (emmitExpr >=> push_) (reverse args)
  return eax_
emmitExpr (Neg e) = do
  op <- emmitExpr e >>= immediateToOperand eax_
  neg_ op
  return op
emmitExpr (Not e) = do
  [tlabel, flabel, endlabel] <- makeNLabels 3
  emmitLogic e (Just flabel) (Just tlabel)
  emmitBoolEpilogue tlabel flabel endlabel
emmitExpr (EArithm e1 op e2) = case op of
  Plus -> addsub add_ e1 e2
  Minus -> addsub sub_ e1 e2
  Times -> addsub imul_ e1 e2
  Div -> do
    basediv e1 e2
    return eax_
  Mod -> do
    basediv e1 e2
    mov_ edx_ eax_
    return eax_
  where
    addsub ctr e1 e2 = do
      push_ =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToOperand eax_
      let op2 = edx_
      pop_ op2
      ctr op1 op2
      return op2

    basediv e1 e2 = do
      push_ =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToOperand eax_
      let divisor = ebx_
      pop_ divisor
      cdq_
      idiv_ divisor
emmitExpr e@ELogic {} = do
  [tlabel, flabel, endlabel] <- makeNLabels 3
  emmitLogic e (Just tlabel) (Just flabel)
  emmitBoolEpilogue tlabel flabel endlabel

emmitLogic :: Expr -> Maybe Label -> Maybe Label -> CodeGen ()
emmitLogic (ELogic e1 op e2) ltrue lfalse = case op of
  LTH -> cmpjmp jl_ jge_ ltrue lfalse e1 e2
  LE -> cmpjmp jle_ jg_ ltrue lfalse e1 e2
  GTH -> cmpjmp jg_ jle_ ltrue lfalse e1 e2
  GE -> cmpjmp jge_ jl_ ltrue lfalse e1 e2
  EQU -> cmpjmp je_ jne_ ltrue lfalse e1 e2
  NE -> cmpjmp jne_ je_ ltrue lfalse e1 e2
  AND -> do
    emmitLogic e1 Nothing lfalse
    emmitLogic e2 ltrue lfalse
  OR -> do
    emmitLogic e1 ltrue Nothing
    emmitLogic e2 ltrue lfalse
emmitLogic (Not e) ltrue lfalse = emmitLogic e lfalse ltrue

cmpjmp instr1 instr2 label1 label2 e1 e2 = do
  push_ =<< emmitExpr e2
  op1 <- emmitExpr e1 >>= immediateToOperand eax_
  let op2 = Register EBX Nothing
  pop_ op2
  cmp_ op1 op2
  jmpIfJust instr1 label1
  jmpIfJust instr2 label2

jmpIfJust instr label = case label of
  (Just label) -> instr label
  _ -> return ()

emmitBoolEpilogue tlabel flabel endlabel = do
  label_ tlabel
  mov_ (Register EAX Nothing) (Const 1)
  jmp_ endlabel
  label_ flabel
  mov_ (Register EAX Nothing) (Const 0)
  jmp_ endlabel
  label_ endlabel
  return $ Register EAX Nothing

addVar :: String -> Operand -> CodeGen (Store -> Store)
addVar n o = do
  push_ o
  addr <- asks _stackH
  return $ over vars (Map.insert n $ addr + 4) . over stackH (+ 4)

emmitStmt :: Stmt -> CodeGen ()
emmitStmt (Block ss) =
  emmitStmtList ss
  where
    emmitStmtList :: [Stmt] -> CodeGen ()
    emmitStmtList ((Decl t n e) : ss) = do
      f <- emmitExpr e >>= addVar n
      local f (emmitStmtList ss)
    emmitStmtList (s : ss) = do
      emmitStmt s
      emmitStmtList ss
    emmitStmtList [] = return ()
-- Decl is handled by Block
emmitStmt (Ass e1 e2) = do
  emmitExpr e2 >>= push_
  emmitExpr e1 >>= pop_
emmitStmt (Incr e) = emmitExpr e >>= inc_
emmitStmt (Decr e) = emmitExpr e >>= dec_
emmitStmt (Ret e) = do
  emmitExpr e >>= movIfNescessary eax_
  leave_ >> ret_
emmitStmt VRet = leave_ >> ret_
emmitStmt (Cond e s) = do
  [tlabel, flabel] <- makeNLabels 2
  emmitLogic e (Just tlabel) (Just flabel)
  label_ tlabel
  emmitStmt s
  label_ flabel
emmitStmt (CondElse e s1 s2) = do
  [tlabel, flabel, endlabel] <- makeNLabels 3
  emmitLogic e (Just tlabel) (Just flabel)
  label_ tlabel
  emmitStmt s1
  jmp_ endlabel
  label_ flabel
  emmitStmt s2
  label_ endlabel
emmitStmt (While e s) = do
  [tlabel, flabel, condlabel] <- makeNLabels 3
  label_ condlabel
  emmitLogic e (Just tlabel) (Just flabel)
  label_ tlabel
  emmitStmt s
  jmp_ condlabel
  label_ flabel
-- TODO
emmitStmt (For t n e s) = undefined
emmitStmt (SExp e) = void $ emmitExpr e

emmitProgram :: Program -> CodeGen ()
emmitProgram (Program defs) = mapM_ emmitTopDef defs

emmitTopDef :: TopDef -> CodeGen ()
emmitTopDef (TopClassDef cls) = undefined
emmitTopDef (TopFnDef (FnDef type_ name args stmt)) = do
  let pos = (* (-4)) <$> [1 ..]
  let f = compose $ zipWith argpos args pos
  label_ name
  enter_
  local f (emmitStmt stmt)
  return ()
  where
    compose = foldr (.) id
    argpos (TypedId _ name) p = over vars (Map.insert name p)

enter_ :: CodeGen ()
enter_ = do
  push_ ebp_
  mov_ (Register ESP Nothing) ebp_

leave_ :: CodeGen ()
leave_ = do
  mov_ ebp_ (Register ESP Nothing)
  pop_ ebp_

makeNLabels :: Integer -> CodeGen [Label]
makeNLabels n = mapM (const makeLabel) [1 .. n]

makeLabel :: CodeGen Label
makeLabel = do
  a <- get
  modify (+ 1)
  return $ "__label__" ++ show a
