{-# LANGUAGE LambdaCase #-}
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( ReaderT,
    ask,
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
    execWriterT,
    runWriter,
    tell,
  )
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import System.IO

type CodeGen d = SRW Store Env d

type ClassMap =
  Map.Map Id (Map.Map Id (Offset, Type), Map.Map Id (Offset, Type))

type StringMap = Map.Map Id Label

type VarMap = Map.Map Id (Operand, Type)

type FunMap = Map.Map Id Type

data Env = Env
  { _vars :: VarMap,
    _stackH :: Integer,
    _inclass :: Maybe Id
  }
  deriving (Show)

data Store = Store
  { _funs :: FunMap,
    _labelCounter :: Integer,
    _stringMap :: StringMap,
    _classes :: ClassMap
  }
  deriving (Show)

$(makeLenses ''Env)
$(makeLenses ''Store)

instance MonadFail Identity where
  fail = error "Fail"

-- runCodeGen :: Program -> ([Instruction], [StringLiteral], [VTable])
runCodeGen program =
  execWriterT $
    runReaderT
      ( evalStateT
          (emmitProgram program)
          ( Store
              { _funs = Map.fromList builtInFuns,
                _labelCounter = 0,
                _stringMap = Map.empty,
                _classes = Map.empty
              }
          )
      )
      (Env {_vars = Map.empty, _stackH = 0, _inclass = Nothing})
  where
    builtInFuns =
      [ ("readString", TypeStr),
        ("readInt", TypeInt),
        ("printString", Void),
        ("printInt", Void),
        ("error", Void)
      ]

immediateToOperand :: Operand -> Operand -> CodeGen Operand
immediateToOperand r c@(Const _) = do
  mov_ c r
  return r
immediateToOperand r o = return o

immediateToOperandType o2 (o1, t) = do
  o2' <- immediateToOperand o2 o1
  return (o2', t)

moveToRegister :: Operand -> Operand -> CodeGen Operand
moveToRegister t@(Register a) f@(Register b) = do
  when (a /= b) (mov_ f t)
  return t
moveToRegister t f = do
  mov_ f t
  return t

moveToRegisterType o2 (o1, t) = do
  o2' <- moveToRegister o2 o1
  return (o2', t)

movIfNescessary :: Operand -> Operand -> CodeGen Operand
movIfNescessary o1 o2 =
  if o1 == o2
    then return o1
    else do
      mov_ o2 o1
      return o1

getVar :: String -> CodeGen (Operand, Type)
getVar n = do
  a <- asks (Map.lookup n . _vars)
  case a of
    Just a -> return a
    Nothing -> do
      -- self is stored as first argument of method call
      -- so it must be under 8(%ebp)
      cls <- asks (fromJust . _inclass)
      (offset, t) <- gets ((Map.! n) . fst . (Map.! cls) . _classes)
      mov_ (Memory EBP (Just 8)) eax_
      return (Memory EAX (Just offset), t)

-- overwrites eax
getFun :: String -> CodeGen (Operand, Type)
getFun n = do
  cls <- asks _inclass
  case cls of
    Just cls -> do
      method <- gets (Map.lookup n . snd . (Map.! cls) . _classes)
      case method of
        -- self is stored as first argument of method call
        -- so it must be under 8(%ebp)
        Just (offset, t) -> do
          mov_ (Memory EBP (Just 8)) eax_
          mov_ (Memory EAX Nothing) eax_
          mov_ (Memory EAX (Just offset)) eax_
          return (Dereference EAX, t)
        Nothing -> simpleFun
    Nothing -> simpleFun
  where
    simpleFun :: CodeGen (Operand, Type)
    simpleFun = do
      t <- gets ((Map.! n) . _funs)
      return (Label n, t)

ensureInRegister :: Operand -> CodeGen Register
ensureInRegister m@(Memory _ _) = do
  mov_ m eax_
  return EAX
ensureInRegister (Register r) = return r

-- result of Expr is packed in eax register or is a constant
emmitExpr :: Expr -> CodeGen (Operand, Type)
emmitExpr (ENewArray t e) = undefined
emmitExpr (ENewObject t@(TypeClass n)) = do
  call_ $ Label (n ++ "__new")
  return (eax_, t)
emmitExpr (EField e i) = do
  (op', t@(TypeClass tn)) <- emmitExpr e
  a <- get
  b <- ask
  liftIO $ print a
  liftIO $ print b
  op <- ensureInRegister op'
  (offset, ftype) <- gets ((Map.! i) . fst . (Map.! tn) . _classes)
  return (Memory op $ Just offset, ftype)
emmitExpr (EMethodCall e i args) = do
  mapM_ (emmitExpr >=> push_ . fst) (reverse args)
  (op', t@(TypeClass tn)) <- emmitExpr e
  (offset, ftype) <- gets ((Map.! i) . snd . (Map.! tn) . _classes)
  op <- ensureInRegister op'
  push_ $ Register op
  mov_ (Memory op Nothing) (Register op) -- vtable address
  unless (offset == 0) $ add_ (Const offset) (Register op)
  call_ $ Dereference op
  add_ (Const (toInteger $ 4 + length args * 4)) esp_
  return (eax_, ftype)
emmitExpr (ELitInt i) = return (Const i, TypeInt)
emmitExpr (EString s) = do
  l <- insertString s
  return (Label l, TypeStr)
emmitExpr (EVar v) = getVar v
emmitExpr (EApp n args) = do
  (op, t) <- getFun n
  let args' = case op of
        (Label _) -> args
        _ -> EVar "self" : args
  mapM_ (emmitExpr >=> push_ . fst) (reverse args')
  call_ op
  add_ (Const (toInteger $ length args * 4)) esp_
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
      (op1, t) <- emmitExpr e1 >>= moveToRegisterType eax_
      case t of
        TypeStr -> do
          push_ op1
          call_ $ Label fConcatString
          add_ (Const 8) esp_
          return (eax_, TypeStr)
        _ -> do
          let op2 = edx_
          pop_ op2
          ctr op2 op1
          return (op1, TypeInt)

    basediv e1 e2 = do
      push_ . fst =<< emmitExpr e2
      op1 <- emmitExpr e1 >>= immediateToOperand eax_ . fst
      when (op1 /= eax_) (mov_ op1 eax_)
      cdq_
      let divisor = ecx_
      pop_ divisor
      -- division result stays in eax
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
emmitLogic e ltrue lfalse = emmitLogic (ELogic e EQU (ELitInt 1)) ltrue lfalse

cmpjmp instr1 instr2 label1 label2 e1 e2 = do
  push_ . fst =<< emmitExpr e2
  op1 <- emmitExpr e1 >>= immediateToOperand eax_ . fst
  let op2 = ecx_
  pop_ op2
  cmp_ op2 op1
  jmpIfJust instr1 label1
  jmpIfJust instr2 label2

jmpIfJust instr label = case label of
  (Just label) -> instr label
  _ -> return ()

emmitBoolEpilogue tlabel flabel endlabel = do
  label_ tlabel
  mov_ (Const 1) eax_
  jmp_ endlabel
  label_ flabel
  mov_ (Const 0) eax_
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
        (\x -> x - 4)

emmitStmt :: Stmt -> CodeGen ()
emmitStmt (Block ss) = do
  emmitStmtList ss
  let nd = numDecl ss
  when (nd /= 0 && not (isRetBlock ss)) (add_ (Const $ nd * 4) esp_)
  where
    emmitStmtList :: [Stmt] -> CodeGen ()
    emmitStmtList ((Decl t n e) : ss) = do
      f <- emmitExpr e >>= uncurry (addVar n)
      local f (emmitStmtList ss)
    emmitStmtList (s : ss) = do
      emmitStmt s
      emmitStmtList ss
    emmitStmtList [] = return ()
    numDecl a =
      toInteger $
        length $
          filter
            ( \case
                Decl {} -> True
                _ -> False
            )
            a
    isRetBlock :: [Stmt] -> Bool
    isRetBlock = _isRetBlock . reverse
    _isRetBlock (VRet : _) = True
    _isRetBlock ((Ret _) : _) = True
    _isRetBlock _ = False
-- Decl is handled by Block
emmitStmt (Ass e1 e2) = do
  -- consts does not need to be pushed and then poped
  -- just mov them
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
  let functions = foldr toFun [] defs
  let classes = foldr toClass [] defs
  let f = compose $ map (\(FnDef t n _ _) -> Map.insert n t) functions
  modify (over funs f)
  emmitClasses classes
  mapM_ emmitFnDef functions
  where
    isFun TopFnDef {} = True
    isFun _ = False
    toFun (TopFnDef f) rest = f : rest
    toFun _ rest = rest
    toClass (TopClassDef c) rest = c : rest
    toClass _ rest = rest

emmitClassMethods :: [ClassDef] -> CodeGen ()
emmitClassMethods = mapM_ emmitMethods
  where
    emmitMethods :: ClassDef -> CodeGen ()
    emmitMethods cls = mapM_ (emmitMethod (getClassname cls)) (getMethods cls)
    emmitMethod :: Id -> FnDef -> CodeGen ()
    emmitMethod clsn (FnDef t funn args body) = do
      local (over inclass $ const (Just clsn)) $
        emmitFnDef
          ( FnDef
              t
              (clsn ++ "__" ++ funn)
              (TypedId (TypeClass clsn) "self" : args)
              body
          )

emmitClasses :: [ClassDef] -> CodeGen ()
emmitClasses s = do
  liftIO $ print classHierarchy
  f <- compose <$> mapM (traverseClassTree classHierarchy Map.empty 0 []) baseclasses
  modify (over classes f)
  a <- get
  liftIO $ print a
  emmitClassMethods s
  where
    isBaseclass :: ClassDef -> Bool
    isBaseclass Class {} = True
    isBaseclass _ = False
    childClass (Class n _) = Map.insertWith (++) n []
    childClass c@(ClassInh b i _) =
      Map.insertWith (++) i [c] . Map.insertWith (++) b []
    name (Class n _) = n
    name (ClassInh n _ _) = n
    comp c1 c2 = compare (name c1) (name c2)

    baseclasses = filter isBaseclass s
    classmap = foldr childClass Map.empty s
    pairsList =
      zipWith (\x (_, ss) -> (x, ss)) (sortBy comp s) (Map.assocs classmap)
    classHierarchy = Map.fromList pairsList

traverseClassTree ::
  Map.Map ClassDef [ClassDef] ->
  Map.Map Id (Id, Integer, Type) ->
  Integer ->
  [TypedId] ->
  ClassDef ->
  CodeGen (ClassMap -> ClassMap)
traverseClassTree m imethods nummeth ifield cls = do
  -- gen vtable
  let newFields = ifield ++ fields
  let (newVTableMap, newNumMeth) = updateVTable methods imethods nummeth
  emmitClassConstructor
    (name ++ "__new")
    (if null newVTableMap then Const 0 else Label $ name ++ "__VTable")
    newFields
  unless (null newVTableMap) $ emmitVTable name newVTableMap

  let methodsMap = Map.map (\(a, b, c) -> (b, c)) newVTableMap
  let fieldsMap =
        Map.fromList $
          zip
            (map (\(TypedId t n) -> n) newFields)
            (zip ((* 4) <$> [1 ..]) (map (\(TypedId t n) -> t) newFields))
  let classMap = Map.insert name (fieldsMap, methodsMap)
  childrenMap <-
    mapM
      (traverseClassTree m newVTableMap newNumMeth newFields)
      children
  return (compose $ classMap : childrenMap)
  where
    name = getClassname cls
    fields = getFields cls
    methods = getMethods cls
    children = m Map.! cls

    updateVTable ::
      [FnDef] ->
      Map.Map Id (Id, Integer, Type) ->
      Integer ->
      (Map.Map Id (Id, Integer, Type), Integer)
    updateVTable fndefs vtable maxaddr = foldl updater (vtable, maxaddr) fndefs
    updater (m, i) (FnDef t n _ _) = case Map.lookup n m of
      Just (_, offset, t) -> (Map.insert n (name, offset, t) m, i)
      Nothing -> (Map.insert n (name, i, t) m, i + 4)

emmitVTable :: Id -> Map.Map Id (Id, Integer, Type) -> CodeGen ()
emmitVTable cls m = do
  let methods =
        map (\(m, (c, _, _)) -> c ++ "__" ++ m) $
          sortBy comparator $
            Map.assocs
              m
  tellVTable $ VTable cls methods
  where
    comparator (_, (_, _, n1)) (_, (_, _, n2)) = compare n1 n2

emmitClassConstructor :: Id -> Operand -> [TypedId] -> CodeGen ()
emmitClassConstructor n v s = do
  label_ n
  push_ ebx_
  push_ $ Const (4 + toInteger (4 * length s))
  call_ $ Label "__malloc"
  add_ (Const 4) esp_
  mov_ eax_ ebx_
  mov_ v (Memory EBX (Just 0))
  let adresses = map (Memory EBX . Just . (* 4)) [1 ..]
  zipWithM_ emmitTypedId adresses s
  mov_ ebx_ eax_
  pop_ ebx_
  ret_
  return ()
  where
    emmitTypedId :: Operand -> TypedId -> CodeGen ()
    emmitTypedId o (TypedId TypeInt _) = mov_ (Const 0) o
    emmitTypedId o (TypedId TypeBool _) = mov_ (Const 0) o
    emmitTypedId o (TypedId TypeStr _) = do
      l <- insertString ""
      mov_ (Label l) o
    emmitTypedId o (TypedId (TypeClass n) _) = do
      call_ $ Label $ n ++ "__new"
      mov_ eax_ o
    emmitTypedId o (TypedId (TypeArray _) _) = undefined

emmitTopDef :: TopDef -> CodeGen ()
emmitTopDef (TopClassDef cls) = undefined
emmitTopDef (TopFnDef fndef) = emmitFnDef fndef

emmitFnDef :: FnDef -> CodeGen ()
emmitFnDef (FnDef type_ name args stmt) = do
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

-- cumpile :: Program -> [String]
-- cumpile program = ".data" : strings ++ vtable ++ textPrologue ++ instrs
--   where
--     (instrs', strings', vtable') = runCodeGen program
--     strings = map show strings'
--     instrs = map show instrs'
--     vtable = map show vtable'
--     textPrologue = [".text", ".globl main"]

cumpile :: Program -> IO [String]
cumpile program = do
  (instrs', strings', vtable') <- runCodeGen program
  let strings = map show strings'
  let instrs = map show instrs'
  let vtable = map show vtable'
  return $ ".data" : strings ++ vtable ++ textPrologue ++ instrs
  where
    textPrologue = [".text", ".globl main"]

fConcatString :: String
fConcatString = "__concat_string"
