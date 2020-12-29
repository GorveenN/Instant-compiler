{-# LANGUAGE TemplateHaskell #-}

module Compiler.Backend.Backend where

import Compiler.Backend.CodeGen
import Compiler.Backend.Tree
import Control.Lens (makeLenses)
import Control.Lens hiding
  ( Const,
    Empty,
    element,
  )
import Control.Monad
import Control.Monad.Fail
  ( MonadFail,
    fail,
  )
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
    gets,
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

type CodeGen d = SRW Store Env d

data Env = Env
  { _vars :: Map.Map String (Operand, Type),
    _stackH :: Integer
  }

data Store = Store
  { _funs :: Map.Map String Type,
    _labelCounter :: Integer,
    _stringMap :: Map.Map String Label
  }

$(makeLenses ''Env)
$(makeLenses ''Store)

instance MonadFail Identity where
  fail = error "Fail"

runCodeGen :: Program -> ([Instruction], [StringLiteral])
runCodeGen program =
  execWriter $
    runReaderT
      ( evalStateT
          (emmitProgram program)
          (Store {_funs = Map.empty, _labelCounter = 0, _stringMap = Map.empty})
      )
      (Env {_vars = Map.empty, _stackH = 0})

immediateToOperand :: Operand -> Operand -> CodeGen Operand
immediateToOperand r c@(Const _) = do
  mov_ c r
  return r
immediateToOperand r o = return o

immediateToOperandType o2 (o1, t) = do
  o2' <- immediateToOperand o2 o1
  return (o2', t)

movIfNescessary :: Operand -> Operand -> CodeGen Operand
movIfNescessary o1 o2 =
  if o1 == o2
    then return o1
    else do
      mov_ o2 o1
      return o1

getVar :: String -> CodeGen (Operand, Type)
getVar n = asks ((Map.! n) . _vars)

-- result of Expr is packed in eax register or is a constant
emmitExpr :: Expr -> CodeGen (Operand, Type)
emmitExpr (ELitInt i) = return (Const i, TypeInt)
emmitExpr (EString s) = do
  l <- insertString s
  return (Label l, TypeStr)
emmitExpr (EVar v) = getVar v
emmitExpr (EApp n args) = do
  mapM_ (emmitExpr >=> push_ . fst) (reverse args)
  call_ n
  add_ (Const (toInteger $ length args * 4)) esp_
  t <- gets ((Map.! n) . _funs)
  return (eax_, t)
emmitExpr (Neg e) = do
  (op, t) <- emmitExpr e >>= immediateToOperandType eax_
  neg_ op
  return (op, t)
emmitExpr (Not e) = do
  [tlabel, flabel, endlabel] <- makeNLabels 3
  emmitLogic e (Just flabel) (Just tlabel)
  o <- emmitBoolEpilogue tlabel flabel endlabel
  return (o, TypeBool)
emmitExpr (EArithm e1 op e2) = case op of
  Plus -> addsub add_ e1 e2
  Minus -> addsub sub_ e1 e2
  Times -> addsub imul_ e1 e2
  Div -> do
    basediv e1 e2
    return (eax_, TypeInt)
  Mod -> do
    basediv e1 e2
    mov_ edx_ eax_
    return (eax_, TypeInt)
  where
    addsub ctr e1 e2 = do
      push_ . fst =<< emmitExpr e2
      (op1, t) <- emmitExpr e1 >>= immediateToOperandType eax_
      case t of
        TypeBool -> do
          push_ op1
          call_ "__str_concat"
          add_ esp_ (Const 8)
          return (eax_, TypeStr)
        _ -> do
          let op2 = edx_
          pop_ op2
          ctr op1 op2
          return (op2, TypeInt)

    basediv e1 e2 = do
      push_ . fst =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToOperand eax_ . fst
      let divisor = ebx_
      pop_ divisor
      cdq_
      idiv_ divisor
emmitExpr e@ELogic {} = do
  [tlabel, flabel, endlabel] <- makeNLabels 3
  emmitLogic e (Just tlabel) (Just flabel)
  op <- emmitBoolEpilogue tlabel flabel endlabel
  return (op, TypeBool)

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
  push_ . fst =<< emmitExpr e2
  op1 <- emmitExpr e1 >>= immediateToOperand eax_ . fst
  let op2 = ebx_
  pop_ op2
  cmp_ op1 op2
  jmpIfJust instr1 label1
  jmpIfJust instr2 label2

jmpIfJust instr label = case label of
  (Just label) -> instr label
  _ -> return ()

emmitBoolEpilogue tlabel flabel endlabel = do
  label_ tlabel
  mov_ eax_ (Const 1)
  jmp_ endlabel
  label_ flabel
  mov_ eax_ (Const 0)
  jmp_ endlabel
  label_ endlabel
  return eax_

addVar :: String -> Operand -> Type -> CodeGen (Env -> Env)
addVar n o t = do
  push_ o
  addr <- asks _stackH
  return $
    over vars (Map.insert n (Memory EBP (Just $ addr - 4), t))
      . over
        stackH
        ((-) 4)

emmitStmt :: Stmt -> CodeGen ()
emmitStmt (Block ss) = emmitStmtList ss
  where
    emmitStmtList :: [Stmt] -> CodeGen ()
    emmitStmtList ((Decl t n e) : ss) = do
      f <- emmitExpr e >>= uncurry (addVar n)
      local f (emmitStmtList ss)
    emmitStmtList (s : ss) = do
      emmitStmt s
      emmitStmtList ss
    emmitStmtList [] = return ()
-- Decl is handled by Block
emmitStmt (Ass e1 e2) = do
  emmitExpr e2 >>= push_ . fst
  emmitExpr e1 >>= pop_ . fst
emmitStmt (Incr e) = emmitExpr e >>= inc_ . fst
emmitStmt (Decr e) = emmitExpr e >>= dec_ . fst
emmitStmt (Ret e) = do
  emmitExpr e >>= movIfNescessary eax_ . fst
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
emmitStmt (For t n e s) = undefined
emmitStmt (SExp e) = void $ emmitExpr e

compose = foldr (.) id

emmitProgram :: Program -> CodeGen ()
emmitProgram (Program defs) = do
  let fd = filter isFun defs
  let f = compose $ map (\(TopFnDef (FnDef t n _ _)) -> Map.insert n t) fd
  modify (over funs f)
  mapM_ emmitTopDef defs
  where
    isFun TopFnDef {} = True
    isFun _ = False

emmitTopDef :: TopDef -> CodeGen ()
emmitTopDef (TopClassDef cls) = undefined
emmitTopDef (TopFnDef (FnDef type_ name args stmt)) = do
  let pos = (* 4) <$> [2 ..]
  let f = compose $ zipWith argpos args pos
  label_ name
  enter_
  local (f . over stackH (const 0)) (emmitStmt stmt)
  where
    argpos (TypedId type_ name) p =
      over vars (Map.insert name (Memory EBP $ Just p, type_))

enter_ :: CodeGen ()
enter_ = do
  push_ ebp_
  mov_ esp_ ebp_

leave_ :: CodeGen ()
leave_ = do
  mov_ ebp_ esp_
  pop_ ebp_

insertString :: String -> CodeGen Label
insertString s = do
  m <- gets _stringMap
  case Map.lookup s m of
    (Just l) -> return l
    Nothing -> do
      l <- makeLabel
      tellStringLiteral (StringLiteral s l)
      modify (over stringMap (Map.insert s l))
      return l

makeNLabels :: Integer -> CodeGen [Label]
makeNLabels n = replicateM (fromInteger n) makeLabel

makeLabel :: CodeGen Label
makeLabel = do
  a <- gets _labelCounter
  modify (over labelCounter (+ 1))
  return $ "__label__" ++ show a

cumpile :: Program -> [String]
cumpile program = ".data" : strings ++ textPrologue ++ instrs
  where
    (instrs', strings') = runCodeGen program
    strings = map show strings'
    instrs = map show instrs'
    textPrologue = [".text", ".globl main"]

fConcatString :: String
fConcatString = "__concat_string"