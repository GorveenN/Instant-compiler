{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Frontend.Frontend where

import AbsLatte
import qualified Compiler.Backend.Tree as T
import Compiler.Frontend.Types
import Compiler.Frontend.Utils
import Control.Lens hiding
  ( Empty,
    element,
  )
import Control.Lens.TH
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import ErrM

type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

checkIdent :: Ident -> String
checkIdent x = case x of
  Ident string -> string

checkProgram :: Show a => Program a -> ERT a ()
checkProgram (Program _ topdefs) = checkList checkTopDef topdefs

checkList :: Show a => (b -> ERT a (StaticEnv -> StaticEnv)) -> [b] -> ERT a ()
checkList fun (s : ss) = do
  mod <- fun s
  local mod (checkList fun ss)
checkList _ [] = do
  return ()

checkTopDef :: Show a => TopDef a -> ERT a (StaticEnv -> StaticEnv)
checkTopDef x = case x of
  TopClassDef _ classdef -> checkClassDef classdef
  TopFunDef _ fndef -> checkFnDef fndef

checkFnDef :: Show a => FnDef a -> ERT a (StaticEnv -> StaticEnv)
checkFnDef x = case x of
  FnDef _ type_ ident_ args_ block_ -> do
    args <- checkArgList args_
    checkType type_
    lvl <- asks _nestLvl
    let argF = foldr ((.) . (\(t, i) -> Map.insert i (t, lvl + 1))) id args
    local (over nestLvl (+ 2) . over varMap argF) (checkBlock block_)
    return (over funMap $ Map.insert ident_ lvl)

checkArgList :: Show a => [Arg a] -> ERT a [(T.Type, Ident)]
checkArgList args = do
  argst <- mapM checkArg args
  let idents = map (snd . fst) argst
  checkIdentUnique $ zip idents (map snd argst)
  return $ map fst argst

checkArg :: Show a => Arg a -> ERT a ((T.Type, Ident), a)
checkArg (Arg pos nonvoidtype ident) = do
  t <- checkNonVoidType nonvoidtype
  return ((t, ident), pos)

detectInheritanceCycle :: Show a => Ident -> ERT a Bool
detectInheritanceCycle name = superclass (T.TypeClass name) (T.TypeClass name)

checkClassMember :: Show a => ClassMember a -> ERT a (StaticEnv -> StaticEnv)
checkClassMember x = case x of
  ClassField pos type_ idents -> do
    ttype <- checkType type_
    let zipped = zip idents $ repeat pos
    lvl <- asks _nestLvl
    checkIdentUnique zipped
    mapM_ (uncurry throwIfMethodDefined) zipped
    mapM_ (uncurry throwIfVariableDefined) zipped
    let functions =
          map (\x -> over varMap (Map.insert x (ttype, lvl))) idents
    return $ foldr (.) id functions
  ClassMethod _ fndef -> checkFnDef fndef

checkClassBlock :: Show a => ClassBlock a -> ERT a (StaticEnv -> StaticEnv)
checkClassBlock x = case x of
  ClassBlock _ classmembers -> do
    let fields = filter isField classmembers
    let methods = filter isField classmembers
    checkList checkClassMember $ fields ++ methods
    return id

checkClassDef :: Show a => ClassDef a -> ERT a (StaticEnv -> StaticEnv)
checkClassDef x = case x of
  Class pos name classblock -> do
    throwIfClassDefined name pos
    checkClassBlock classblock
    return (over classMap (Set.insert name))
  ClassInh pos name supername classblock -> do
    -- TODO run with all parent methods/field
    throwIfClassDefined name pos
    cycle <- detectInheritanceCycle name
    when cycle $ throwError $ CyclicInheritance pos name
    (fields, methods) <- superFieldsMethods supername
    lvl <- asks _nestLvl
    -- let fieldsF = map (\(i, t) -> over varMap (Map.insert i (t, lvl + 1))) fields
    let fieldsF = foldr ((.) . \(i, t) -> over varMap (Map.insert i (t, lvl + 1))) id fields
    let methodsF = foldr ((.) . \i -> over funMap (Map.insert i (lvl + 1))) id methods
    local (fieldsF . methodsF) $ checkClassBlock classblock
    return (over classMap (Set.insert name))

superFieldsMethods :: Show a => Ident -> ERT a ([(Ident, T.Type)], [Ident])
superFieldsMethods name = do
  a <- gets (Map.lookup name . _allClasses)
  case a of
    Just ClassMeta {_super = Just super} -> _superFieldsMethods super
    _ -> return ([], [])
  where
    _superFieldsMethods :: Show a => Ident -> ERT a ([(Ident, T.Type)], [Ident])
    _superFieldsMethods name = do
      a <- gets (Map.lookup name . _allClasses)
      case a of
        Just ClassMeta {_fields = fields, _methods = methods, _super = Just super} ->
          do
            let f = Map.assocs fields
            let m = Map.keys methods
            (restf, restm) <- _superFieldsMethods super
            return (f ++ restf, m ++ restm)
        _ -> return ([], [])

superclass :: Show a => T.Type -> T.Type -> ERT a Bool
superclass (T.TypeClass t1) (T.TypeClass t2) = do
  a <- gets (Map.lookup t1 . _allClasses)
  case a of
    Just ClassMeta {_super = Just super} -> do
      if super == t2
        then return True
        else superclass (T.TypeClass super) (T.TypeClass t2)
    _ -> return False
superclass _ _ = do
  return False

isField x = case x of
  (ClassMethod _ _) -> True
  _ -> False

isMethod = not . isField

checkStmtDecl :: Show a => Stmt a -> [Stmt a] -> ERT a Bool
checkStmtDecl (Decl _ nonvoidtype items) ss = do
  t <- checkNonVoidType nonvoidtype
  checkDeclList t items ss

checkDeclList :: Show a => T.Type -> [Item a] -> [Stmt a] -> ERT a Bool
checkDeclList t (i : ii) ss = do
  f <- checkItem t i
  local f (checkDeclList t ii ss)
checkDeclList _ [] ss = checkStmtList ss

checkItem :: Show a => T.Type -> Item a -> ERT a (StaticEnv -> StaticEnv)
checkItem t x = case x of
  NoInit _ ident -> do
    lvl <- asks _nestLvl
    return (over varMap (Map.insert ident (t, lvl)))
  Init pos ident expr -> do
    exprt <- checkExpr expr
    unless (t == exprt) $ throwError $ TypeMismatch pos t exprt
    lvl <- asks _nestLvl
    return (over varMap (Map.insert ident (t, lvl)))

checkBlock :: Show a => Block a -> ERT a Bool
checkBlock (Block _ stmts) = local (over nestLvl (+ 1)) (checkStmtList stmts)

checkStmtList :: Show a => [Stmt a] -> ERT a Bool
checkStmtList (decl@(Decl {}) : ss) = checkStmtDecl decl ss
checkStmtList (s : ss) = liftM2 (||) (checkStmt s) (checkStmtList ss)
checkStmtList [] = return False

-- superclass :: Show a => T.Type -> T.Type -> ERT a Bool
-- superclass (T.TypeClass t1) (T.TypeClass t2) = do
--     a <- gets (Map.lookup t1 . _allClasses)
--     case a of
--         Just ClassMeta { _super = Just super } -> do
--             if super == t2
--                 then return True
--                 else superclass (T.TypeClass super) (T.TypeClass t2)
--         _ -> return False
-- superclass _ _ = do
--     return False

isLvalue :: Expr a -> Bool
isLvalue EField {} = True
isLvalue EVar {} = True
isLvalue _ = False

checkStmt :: Show a => Stmt a -> ERT a Bool
checkStmt x = case x of
  Empty _ -> return False
  BStmt _ block -> checkBlock block
  Ass pos expr1 expr2 -> do
    unless (isLvalue expr1) $ throwError (AssignmentToRValue pos)
    expr1t <- checkExpr expr1
    expr2t <- checkExpr expr2
    issuper <- expr2t `superclass` expr1t
    unless (expr1t == expr2t || issuper) $
      throwError $
        TypeMismatch
          pos
          expr1t
          expr2t
    return False
  Incr pos ident -> do
    throwIfWrongType (EVar pos ident) T.TypeInt
    throwIfVariableNotDefined ident pos
    return False
  Decr pos ident -> do
    checkStmt (Incr pos ident)
  Ret _ expr -> do
    rett <- asks _retType
    throwIfWrongType expr rett
    return True
  VRet pos -> do
    rett <- asks _retType
    unless (rett == T.Void) $ throwError $ TypeMismatch pos rett T.Void
    return True
  Cond pos expr stmt -> do
    checkStmt (CondElse pos expr stmt (Empty pos))
  CondElse _ expr stmt1 stmt2 -> do
    throwIfWrongType expr T.TypeBool
    liftM2 (&&) (checkStmt stmt1) (checkStmt stmt2)
  While _ expr stmt -> do
    throwIfWrongType expr T.TypeBool
    checkStmt stmt
    return False
  For pos nonvoidtype ident expr stmt -> do
    typet <- checkNonVoidType nonvoidtype
    exprt <- checkExpr expr
    case exprt of
      (T.TypeArray a) -> do
        unless (a == typet) $
          throwError $
            TypeMismatch pos typet $
              T.TypeArray a
      _ -> throwError $ TypeMismatch pos exprt $ T.TypeArray typet
    lvl <- asks _nestLvl
    local
      ( over varMap (Map.insert ident (typet, lvl + 1))
          . over nestLvl (+ 2)
      )
      (checkStmt stmt)
    return False
  SExp _ expr -> do
    checkExpr expr
    return False

checkScalarType :: Show a => ScalarType a -> ERT a T.Type
checkScalarType x = case x of
  ClassType pos ident -> do
    throwIfClassNotDefined ident pos
    return $ T.TypeClass ident
  Int _ -> return T.TypeInt
  Str _ -> return T.TypeStr
  Bool _ -> return T.TypeBool

checkNonVoidType :: Show a => NonVoidType a -> ERT a T.Type
checkNonVoidType x = case x of
  ArrayType _ scalartype -> do
    a <- checkScalarType scalartype
    return $ T.TypeArray a
  ScalarType _ scalartype -> checkScalarType scalartype

checkType :: Show a => Type a -> ERT a T.Type
checkType x = case x of
  NonVoidType _ nonvoidtype -> checkNonVoidType nonvoidtype
  Void _ -> return T.Void

checkExpr :: Show a => Expr a -> ERT a T.Type
checkExpr x = case x of
  ENewObject pos scalar -> do
    t <- checkScalarType scalar
    throwIfTypeNotDefined pos t
    case t of
      (T.TypeClass a) -> do
        throwIfClassNotDefined a pos
        return t
      _ -> throwError $ NewOnNonClassType pos t
    return t
  ENewArray pos inner size -> do
    throwIfWrongType size T.TypeInt
    innert <- checkScalarType inner
    throwIfTypeNotDefined pos innert
    return $ T.TypeArray innert
  EField pos expr ident -> do
    exprt <- checkExpr expr
    getFieldType pos exprt ident
  EMethodCall pos expr ident exprs -> do
    exprt <- checkExpr expr
    (ttype, args) <- getMethodType pos exprt ident
    throwIfArgumentsMismatch pos (map fst args) exprs
    return ttype
  EVar pos ident -> do
    (type_, _) <-
      asks (Map.lookup ident . _varMap)
        >>= lookupFail (VariableNotInScope pos ident)
    return type_
  ELitInt _ _ -> return T.TypeInt
  ELitTrue _ -> return T.TypeBool
  ELitFalse _ -> return T.TypeBool
  EString _ _ -> return T.TypeStr
  EApp pos ident exprs -> do
    (ttype, args) <-
      gets (Map.lookup ident . _allFuns)
        >>= lookupFail (FunctionNotInScope pos ident)
    throwIfArgumentsMismatch pos (map fst args) exprs
    return ttype
  EAccess pos indexed index -> do
    throwIfWrongType index T.TypeInt
    arrType <- checkExpr indexed
    case arrType of
      T.TypeArray a -> return a
      t ->
        throwError $
          TypeMismatch
            pos
            (T.TypeArray (T.TypeClass (Ident "Any")))
            t
  ECast pos ident -> do
    gets (Map.member ident . _allClasses)
      >>= memberFail (SymbolNotDefined pos ident)
    return $ T.TypeClass ident
  Neg _ expr -> throwIfWrongType expr T.TypeInt
  Not _ expr -> throwIfWrongType expr T.TypeBool
  EMul _ expr1 _ expr2 -> throwIfWrong2Types T.TypeInt expr1 expr2
  EAdd pos expr1 _ expr2 -> do
    expr1t <- checkExpr expr1
    expr2t <- checkExpr expr2
    unless
      ( expr1t
          == expr2t
          && (expr1t == T.TypeInt || expr1t == T.TypeStr)
      )
      $ throwError (AddNonAddable pos expr1t expr2t)
    return expr1t
  ERel pos expr1 relop expr2 -> do
    expr1t <- checkExpr expr1
    expr2t <- checkExpr expr2
    unless (expr1t == expr2t) $
      throwError (CompareDifferentTypes pos expr1t expr2t)
    when (not (isEqNeq relop) && not (isInt expr1t)) $
      throwError (NonComparableTypes pos expr1t expr2t)
    return T.TypeBool
  EAnd _ expr1 expr2 -> throwIfWrong2Types T.TypeBool expr1 expr2
  EOr _ expr1 expr2 -> throwIfWrong2Types T.TypeBool expr1 expr2
  where
    throwIfWrong2Types t e1 e2 = do
      throwIfWrongType e1 t
      throwIfWrongType e2 t
    isEqNeq (EQU _) = True
    isEqNeq (NE _) = True
    isEqNeq _ = False

    isInt T.TypeInt = True
    isInt _ = False

getSuperMembers :: Ident -> [ClassMeta]
getSuperMembers = undefined

throwIfWrongType :: Show a => Expr a -> T.Type -> ERT a T.Type
throwIfWrongType expr ttype = do
  t <- checkExpr expr
  unless (t == ttype) $ throwError $ TypeMismatch (exprPosition expr) ttype t
  return t

throwIfTypeNotDefined :: Show a => a -> T.Type -> ERT a ()
throwIfTypeNotDefined p t = do
  defined <- typeDefined t
  unless defined $ throwError (TypeNotDefined p t)

throwIfSymbolNotDefined :: Show a => a -> Ident -> ERT a ()
throwIfSymbolNotDefined = undefined

throwIfArgumentsMismatch ::
  Show a => a -> [T.Type] -> [Expr a] -> ERT a [T.Type]
throwIfArgumentsMismatch pos formal actual = do
  when (length formal /= length actual) $
    throwError $
      WrongNumberOfArguments
        pos
        (length formal)
        (length actual)
  zipWithM
    ( \t expr -> do
        exprType <- checkExpr expr
        when (exprType /= t) $
          throwError (TypeMismatch (exprPosition expr) t exprType)
        return exprType
    )
    formal
    actual

_throwIfSymbolNotDefined f var pos = do
  a <- asks $ Map.lookup var . f
  case a of
    Nothing -> throwError $ SymbolNotDefined pos var
    _ -> return ()

_throwIfSymbolDefined f var pos = do
  a <- asks $ Map.lookup var . f
  case a of
    Just _ -> return ()
    _ -> throwError $ SymbolNotDefined pos var

_throwSymbol f1 f2 var pos = do
  a <- asks $ Map.lookup var . f1
  f2 pos var a

_throwJust :: Show a => a -> Ident -> Maybe b -> ERT a ()
_throwJust pos var (Just _) = throwError $ SymbolNotDefined pos var
_throwJust _ _ Nothing = return ()

_throwNothing _ _ (Just _) = return ()
_throwNothing pos var Nothing = throwError $ SymbolNotDefined pos var

throwIfVariableDefined :: Show a => Ident -> a -> ERT a ()
throwIfVariableDefined = _throwSymbol _varMap _throwJust

throwIfVariableNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfVariableNotDefined = _throwSymbol _varMap _throwNothing

throwIfMethodDefined :: Show a => Ident -> a -> ERT a ()
throwIfMethodDefined = _throwSymbol _funMap _throwJust

throwIfMethodNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfMethodNotDefined = _throwSymbol _funMap _throwNothing

throwIfClassDefined :: Show a => Ident -> a -> ERT a ()
throwIfClassDefined var pos = do
  a <- asks $ Set.member var . _classMap
  unless a $ throwError $ RedefinitionOfSymbol pos var

throwIfClassNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfClassNotDefined var pos = do
  a <- asks $ Set.member var . _classMap
  unless a $ throwError $ RedefinitionOfSymbol pos var

signatureFunction :: Show a => FnDef a -> ERT a (Ident, Function)
signatureFunction = undefined

checkIdentUnique :: Show a => [(Ident, a)] -> ERT a ()
checkIdentUnique list = do
  foldM_ foldFunc [] list
  where
    elemPair e = foldl (\acc x -> acc || (fst x == e)) False
    foldFunc acc (n, p) =
      if n `elemPair` acc
        then throwError (RedefinitionOfSymbol p n)
        else return $ (n, p) : acc

getMethodType :: Show a => a -> T.Type -> Ident -> ERT a Function
getMethodType pos cls@(T.TypeClass className) methodName = do
  ClassMeta {_methods = methodsMap} <-
    gets (Map.lookup className . _allClasses)
      >>= lookupFail (TypeNotDefined pos cls)
  case Map.lookup methodName methodsMap of
    (Just t) -> return t
    _ -> throwError $ MethodNotInScope pos cls methodName

methodType pos className methodName =
  throwError $ MethodNotInScope pos className methodName

getFieldType :: Show a => a -> T.Type -> Ident -> ERT a T.Type
getFieldType pos cls@(T.TypeClass className) fieldName = do
  ClassMeta {_fields = fieldsMap} <-
    gets (Map.lookup className . _allClasses)
      >>= lookupFail (TypeNotDefined pos cls)
  case Map.lookup fieldName fieldsMap of
    (Just t) -> return t
    _ -> throwError $ FieldNotInScope pos cls fieldName

fieldType pos className fieldName =
  throwError $ FieldNotInScope pos className fieldName

typeDefined :: Show a => T.Type -> ERT a Bool
typeDefined (T.TypeArray t) = typeDefined t
typeDefined (T.TypeClass n) = gets $ Map.member n . _allClasses
typeDefined _ = return True

lookupFail err = maybe (throwError err) return

memberFail err True = throwError err
memberFail _ False = return False
