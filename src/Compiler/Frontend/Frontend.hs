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
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

runSRE :: Show a => Program a -> Either (StaticException a) ()
runSRE program@(Program _ topdefs) =
  if Ident "main" `elem` map fst funs
    then
      runIdentity $
        runExceptT
          ( runReaderT
              (evalStateT (checkProgram program) store)
              ( StaticEnv
                  { _varMap = Map.empty,
                    _funMap = Map.fromList scopedFs,
                    _classMap = Set.empty,
                    _retType = T.Void,
                    _nestLvl = 0
                  }
              )
          )
    else Left MainNotDefined
  where
    predefinedFs =
      [ (Ident "printInt", (T.Void, [(T.TypeInt, Ident "x")])),
        (Ident "printString", (T.Void, [(T.TypeStr, Ident "x")])),
        (Ident "error", (T.Void, [])),
        (Ident "readInt", (T.TypeInt, [])),
        (Ident "readString", (T.TypeStr, []))
      ]
    scopedFs :: [(Ident, NestLvl)]
    scopedFs = map (\(n, (_, _)) -> (n, 0)) predefinedFs
    (funs, classes) = signatureTopDefList topdefs
    store =
      ( StaticStore
          { _allFuns = Map.fromList $ predefinedFs ++ funs,
            _allClasses = Map.fromList classes
          }
      )

signatureTopDefList ::
  Show a => [TopDef a] -> ([(Ident, Function)], [(Ident, ClassMeta)])
signatureTopDefList (s : ss) = case s of
  (TopClassDef _ c) -> (functions, signatureClass c : classes)
  (TopFunDef _ f) -> (signatureFunction f : functions, classes)
  where
    (functions, classes) = signatureTopDefList ss
signatureTopDefList [] = ([], [])

checkProgram :: Show a => Program a -> SRE a ()
checkProgram (Program _ topdefs) = do
  checkList checkTopDef topdefs

checkList :: Show a => (b -> SRE a (StaticEnv -> StaticEnv)) -> [b] -> SRE a ()
checkList fun (s : ss) = do
  mod <- fun s
  local mod (checkList fun ss)
checkList _ [] = do
  return ()

checkTopDef :: Show a => TopDef a -> SRE a (StaticEnv -> StaticEnv)
checkTopDef x = case x of
  TopClassDef _ classdef -> checkClassDef classdef
  TopFunDef _ fndef -> checkFnDef fndef

checkFnDef :: Show a => FnDef a -> SRE a (StaticEnv -> StaticEnv)
checkFnDef x = case x of
  FnDef pos type_ ident_ args_ block_ -> do
    args <- checkArgList args_
    rett <- checkType type_
    lvl <- asks _nestLvl
    let argF = foldr ((.) . (\(t, i) -> Map.insert i (t, lvl + 1))) id args
    returns <-
      local
        (over nestLvl (+ 2) . over varMap argF . set retType rett)
        (checkBlock block_)
    when (rett /= T.Void && not returns) $ throwError (NoReturn pos ident_)

    return (over funMap $ Map.insert ident_ lvl)

checkArgList :: Show a => [Arg a] -> SRE a [(T.Type, Ident)]
checkArgList args = do
  argst <- mapM checkArg args
  let idents = map (snd . fst) argst
  checkIdentUnique $ zip idents (map snd argst)
  return $ map fst argst

checkArg :: Show a => Arg a -> SRE a ((T.Type, Ident), a)
checkArg (Arg pos nonvoidtype ident) = do
  t <- checkNonVoidType nonvoidtype
  return ((t, ident), pos)

detectInheritanceCycle :: Show a => Ident -> SRE a Bool
detectInheritanceCycle name = superclass (T.TypeClass name) (T.TypeClass name)

checkClassMember :: Show a => ClassMember a -> SRE a (StaticEnv -> StaticEnv)
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

checkClassBlock :: Show a => ClassBlock a -> SRE a (StaticEnv -> StaticEnv)
checkClassBlock x = case x of
  ClassBlock _ classmembers -> do
    let fields = filter isField classmembers
    let methods = filter isMethod classmembers
    checkList checkClassMember $ fields ++ methods
    return id

checkClassDef :: Show a => ClassDef a -> SRE a (StaticEnv -> StaticEnv)
checkClassDef x = case x of
  Class pos name classblock -> do
    throwIfClassDefined name pos
    lvl <- asks _nestLvl
    let self =
          over
            varMap
            (Map.insert (Ident "self") (T.TypeClass name, lvl + 1))
    local (over nestLvl (+ 1) . self) $ checkClassBlock classblock
    return (over classMap (Set.insert name))
  ClassInh pos name supername classblock -> do
    -- TODO run with all parent methods/field
    throwIfClassDefined name pos
    cycle <- detectInheritanceCycle name
    when cycle $ throwError $ CyclicInheritance pos name
    (fields, methods) <- superFieldsMethods supername
    lvl <- asks _nestLvl
    let fieldsF =
          foldr
            ((.) . \(i, t) -> over varMap (Map.insert i (t, lvl + 1)))
            id
            fields
    let methodsF =
          foldr
            ((.) . \i -> over funMap (Map.insert i (lvl + 1)))
            id
            methods

    let self =
          over
            varMap
            (Map.insert (Ident "self") (T.TypeClass name, lvl + 2))
    local (fieldsF . methodsF . self . over nestLvl (+ 2)) $
      checkClassBlock classblock
    return (over classMap (Set.insert name))

superFieldsMethods :: Show a => Ident -> SRE a ([(Ident, T.Type)], [Ident])
superFieldsMethods name = do
  a <- gets (Map.lookup name . _allClasses)
  case a of
    Just ClassMeta {_super = Just super} -> _superFieldsMethods super
    _ -> return ([], [])
  where
    _superFieldsMethods :: Show a => Ident -> SRE a ([(Ident, T.Type)], [Ident])
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

superclass :: Show a => T.Type -> T.Type -> SRE a Bool
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
  (ClassMethod _ _) -> False
  _ -> True

isMethod = not . isField

checkStmtDecl :: Show a => Stmt a -> [Stmt a] -> SRE a Bool
checkStmtDecl (Decl _ nonvoidtype items) ss = do
  t <- checkNonVoidType nonvoidtype
  checkDeclList t items ss

checkDeclList :: Show a => T.Type -> [Item a] -> [Stmt a] -> SRE a Bool
checkDeclList t (i : ii) ss = do
  f <- checkItem t i
  local f (checkDeclList t ii ss)
checkDeclList _ [] ss = checkStmtList ss

checkItem :: Show a => T.Type -> Item a -> SRE a (StaticEnv -> StaticEnv)
checkItem t x = case x of
  NoInit pos ident -> do
    lvl <- asks _nestLvl
    throwIfVariableDefined ident pos
    return (over varMap (Map.insert ident (t, lvl)))
  Init pos ident expr -> do
    throwIfVariableDefined ident pos
    exprt <- checkExpr expr
    super <- exprt `superclass` t
    unless (t == exprt || super) $ throwError $ TypeMismatch pos t exprt
    lvl <- asks _nestLvl
    return (over varMap (Map.insert ident (t, lvl)))

checkBlock :: Show a => Block a -> SRE a Bool
checkBlock (Block _ stmts) = local (over nestLvl (+ 1)) (checkStmtList stmts)

checkStmtList :: Show a => [Stmt a] -> SRE a Bool
checkStmtList (decl@Decl {} : ss) = checkStmtDecl decl ss
checkStmtList (s : ss) = liftM2 (||) (checkStmt s) (checkStmtList ss)
checkStmtList [] = return False

isLvalue :: Expr a -> Bool
isLvalue EField {} = True
isLvalue EVar {} = True
isLvalue EAccess {} = True
isLvalue _ = False

checkStmt :: Show a => Stmt a -> SRE a Bool
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

checkScalarType :: Show a => ScalarType a -> SRE a T.Type
checkScalarType x = case x of
  ClassType pos ident -> do
    throwIfClassNotDefined ident pos
    return $ T.TypeClass ident
  Int _ -> return T.TypeInt
  Str _ -> return T.TypeStr
  Bool _ -> return T.TypeBool

checkNonVoidType :: Show a => NonVoidType a -> SRE a T.Type
checkNonVoidType x = case x of
  ArrayType _ scalartype -> do
    a <- checkScalarType scalartype
    return $ T.TypeArray a
  ScalarType _ scalartype -> checkScalarType scalartype

checkType :: Show a => Type a -> SRE a T.Type
checkType x = case x of
  NonVoidType _ nonvoidtype -> checkNonVoidType nonvoidtype
  Void _ -> return T.Void

checkExpr :: Show a => Expr a -> SRE a T.Type
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

throwIfWrongType :: Show a => Expr a -> T.Type -> SRE a T.Type
throwIfWrongType expr ttype = do
  t <- checkExpr expr
  unless (t == ttype) $ throwError $ TypeMismatch (exprPosition expr) ttype t
  return t

throwIfTypeNotDefined :: Show a => a -> T.Type -> SRE a ()
throwIfTypeNotDefined p t = do
  defined <- typeDefined t
  unless defined $ throwError (TypeNotDefined p t)

throwIfArgumentsMismatch ::
  Show a => a -> [T.Type] -> [Expr a] -> SRE a [T.Type]
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

_throwJust :: Show a => a -> Ident -> Maybe b -> SRE a ()
_throwJust pos var (Just _) = throwError $ RedefinitionOfSymbol pos var
_throwJust _ _ Nothing = return ()

_throwNothing _ _ (Just _) = return ()
_throwNothing pos var Nothing = throwError $ SymbolNotDefined pos var

throwIfVariableDefined :: Show a => Ident -> a -> SRE a ()
throwIfVariableDefined = _throwSymbol _varMap _throwJust

throwIfVariableNotDefined :: Show a => Ident -> a -> SRE a ()
throwIfVariableNotDefined = _throwSymbol _varMap _throwNothing

throwIfMethodDefined :: Show a => Ident -> a -> SRE a ()
throwIfMethodDefined = _throwSymbol _funMap _throwJust

throwIfClassDefined :: Show a => Ident -> a -> SRE a ()
throwIfClassDefined var pos = do
  a <- asks $ Set.member var . _classMap
  when a $ throwError $ RedefinitionOfSymbol pos var

throwIfClassNotDefined :: Show a => Ident -> a -> SRE a ()
throwIfClassNotDefined var pos = do
  a <- gets $ Map.member var . _allClasses
  unless a $ throwError $ TypeNotDefined pos $ T.TypeClass var

signatureType :: Show a => Type a -> T.Type
signatureType x = case x of
  NonVoidType _ nonvoidtype -> signatureNonVoidType nonvoidtype
  Void _ -> T.Void

signatureScalarType :: Show a => ScalarType a -> T.Type
signatureScalarType x = case x of
  ClassType _ ident -> T.TypeClass ident
  Int _ -> T.TypeInt
  Str _ -> T.TypeStr
  Bool _ -> T.TypeBool

signatureNonVoidType :: Show a => NonVoidType a -> T.Type
signatureNonVoidType x = case x of
  ArrayType _ scalartype -> T.TypeArray $ signatureScalarType scalartype
  ScalarType _ scalartype -> signatureScalarType scalartype

signatureFunction :: Show a => FnDef a -> (Ident, Function)
signatureFunction (FnDef _ t name args _) = (name, (signatureType t, argtypes))
  where
    argtypes = map (\(Arg _ t n) -> (signatureNonVoidType t, n)) args

signatureClass :: Show a => ClassDef a -> (Ident, ClassMeta)
signatureClass x = case x of
  Class _ name (ClassBlock _ members) -> (name, toClassMeta members Nothing)
  ClassInh _ name supername (ClassBlock _ members) ->
    (name, toClassMeta members (Just supername))
  where
    fields mem =
      concatMap
        ( \(ClassField _ t idents) ->
            zip idents (repeat $ signatureType t)
        )
        $ filter isField mem
    methods mem =
      map (\(ClassMethod _ fndef) -> signatureFunction fndef) $
        filter isMethod mem
    toClassMeta ms ss =
      ClassMeta
        { _fields = Map.fromList $ fields ms,
          _methods = Map.fromList $ methods ms,
          _super = ss
        }

checkIdentUnique :: Show a => [(Ident, a)] -> SRE a ()
checkIdentUnique list = do
  foldM_ foldFunc [] list
  where
    elemPair e = foldl (\acc x -> acc || (fst x == e)) False
    foldFunc acc (n, p) =
      if n `elemPair` acc
        then throwError (RedefinitionOfSymbol p n)
        else return $ (n, p) : acc

-- TODO lookup superclass
getMethodType :: Show a => a -> T.Type -> Ident -> SRE a Function
getMethodType pos cls@(T.TypeClass className) methodName = do
  ClassMeta {_methods = methodsMap, _super = super} <-
    gets (Map.lookup className . _allClasses)
      >>= lookupFail (TypeNotDefined pos cls)
  case Map.lookup methodName methodsMap of
    (Just t) -> return t
    _ -> do
      case super of
        Just sname -> getMethodType pos (T.TypeClass sname) methodName
        _ -> throwError $ MethodNotInScope pos cls methodName
getMethodType pos className methodName =
  throwError $ MethodNotInScope pos className methodName

-- TODO lookup superclass
getFieldType :: Show a => a -> T.Type -> Ident -> SRE a T.Type
getFieldType pos cls@(T.TypeClass className) fieldName = do
  ClassMeta {_fields = fieldsMap, _super = super} <-
    gets (Map.lookup className . _allClasses)
      >>= lookupFail (TypeNotDefined pos cls)
  case Map.lookup fieldName fieldsMap of
    (Just t) -> return t
    _ -> do
      case super of
        Just sname -> getFieldType pos (T.TypeClass sname) fieldName
        _ -> throwError $ FieldNotInScope pos cls fieldName
getFieldType pos t@(T.TypeArray _) fieldName =
  if fieldName == Ident "length"
    then return T.TypeInt
    else throwError $ FieldNotInScope pos t fieldName
getFieldType pos type_ fieldName =
  throwError $ FieldNotInScope pos type_ fieldName

typeDefined :: Show a => T.Type -> SRE a Bool
typeDefined (T.TypeArray t) = typeDefined t
typeDefined (T.TypeClass n) = gets $ Map.member n . _allClasses
typeDefined _ = return True

lookupFail err = maybe (throwError err) return

memberFail _ True = return False
memberFail err False = throwError err
