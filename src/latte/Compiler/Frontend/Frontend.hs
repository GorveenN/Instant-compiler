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
import Control.Monad.Extra (ifM)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

data ConstExpr
  = ConstString String
  | ConstInteger Integer
  | ConstBool Bool
  deriving (Eq, Ord)

runSRE :: Show a => Program a -> Either (StaticException a) ()
runSRE program@(Program _ topdefs) =
  if (Ident "main", (T.TypeInt, [])) `elem` funs
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
                    _nestLvl = 0,
                    _inClass = Nothing
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

superMethod :: Ident -> Ident -> SRE a (Maybe Function)
superMethod clsname name = do
  cls <- gets (Map.lookup clsname . _allClasses)
  case cls of
    Just ClassMeta {_super = Just super} -> _superMethod super name
    _ -> return Nothing
  where
    _superMethod clsname name = do
      cls <- gets (Map.lookup clsname . _allClasses)
      case cls of
        Just ClassMeta {_methods = methods, _super = super} -> do
          case Map.lookup name methods of
            Just f -> do
              return $ Just f
            _ -> case super of
              Just supername -> _superMethod supername name
              _ -> return Nothing
        _ -> return Nothing

checkMethodRedefinition pos name ret args = do
  incls <- asks _inClass
  case incls of
    Just clsname -> do
      a <- superMethod clsname name
      case a of
        Just (ft, fargs) -> do
          let f1 = (ret, map fst args)
          let f2 = (ft, map fst fargs)
          unless (f1 == f2) $ throwError (RedefinitionOfSymbol pos name)
        _ -> return ()
    _ -> return ()

checkFnDef :: Show a => FnDef a -> SRE a (StaticEnv -> StaticEnv)
checkFnDef x = case x of
  FnDef pos type_ ident_ args_ block_ -> do
    throwIfFunctionDefined ident_ pos
    args <- checkArgList args_
    rett <- checkType type_
    checkMethodRedefinition pos ident_ rett args
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

constExpr :: Show a => Expr a -> SRE a (Maybe ConstExpr)
constExpr x = case x of
  ELitInt _ i -> do
    return $ Just (ConstInteger i)
  ELitTrue _ -> do
    return $ Just (ConstBool True)
  ELitFalse _ -> do
    return $ Just (ConstBool False)
  EString _ s -> do
    return $ Just (ConstString s)
  Neg _ expr -> do
    e <- constExpr expr
    case e of
      (Just (ConstInteger i)) -> return $ Just (ConstInteger (- i))
      _ -> return Nothing
  Not _ expr -> do
    e <- constExpr expr
    case e of
      (Just (ConstBool i)) -> return $ Just (ConstBool (not i))
      _ -> return Nothing
  EMul _ expr1 mulop expr2 -> do
    e1 <- constExpr expr1
    e2 <- constExpr expr2
    case (e1, e2) of
      (Just (ConstInteger i1), Just (ConstInteger i2)) -> do
        case mulop of
          Times _ -> return $ Just (ConstInteger (i1 * i2))
          Mod _ -> return $ Just (ConstInteger (mod i1 i2))
          Div _ ->
            if i2 == 0
              then return Nothing
              else return $ Just (ConstInteger (div i1 i2))
      _ -> return Nothing
  EAdd _ expr1 addop expr2 -> do
    e1 <- constExpr expr1
    e2 <- constExpr expr2
    case (e1, e2) of
      (Just (ConstInteger i1), Just (ConstInteger i2)) -> do
        case addop of
          Plus _ -> return $ Just (ConstInteger (i1 + i2))
          Minus _ -> return $ Just (ConstInteger (i1 - i2))
      (Just (ConstString i1), Just (ConstString i2)) -> case addop of
        Plus _ -> return $ Just (ConstString (i1 ++ i2))
        _ -> return Nothing
      _ -> return Nothing
  ERel _ expr1 relop expr2 -> do
    e1 <- constExpr expr1
    e2 <- constExpr expr2
    let op = relopToF relop
    case (e1, e2) of
      (Just (ConstInteger i1), Just (ConstInteger i2)) ->
        return $ Just (ConstBool (op i1 i2))
      _ -> return Nothing
  EAnd _ expr1 expr2 -> do
    e1 <- constExpr expr1
    e2 <- constExpr expr2
    case (e1, e2) of
      (Just (ConstBool i1), Just (ConstBool i2)) ->
        return $ Just (ConstBool (i1 && i2))
      _ -> return Nothing
  EOr _ expr1 expr2 -> do
    e1 <- constExpr expr1
    e2 <- constExpr expr2
    case (e1, e2) of
      (Just (ConstBool i1), Just (ConstBool i2)) ->
        return $ Just (ConstBool (i1 || i2))
      _ -> return Nothing
  _ -> return Nothing
  where
    relopToF op = case op of
      LTH _ -> (<)
      LE _ -> (<=)
      GTH _ -> (>)
      GE _ -> (>=)
      EQU _ -> (==)
      NE _ -> (/=)

checkClassMember :: Show a => ClassMember a -> SRE a (StaticEnv -> StaticEnv)
checkClassMember x = case x of
  ClassField pos type_ idents -> do
    ttype <- checkType type_
    when (ttype == T.Void) $ throwError (VoidField pos)
    let zipped = zip idents $ repeat pos
    lvl <- asks _nestLvl
    checkIdentUnique zipped
    mapM_ (uncurry throwIfFunctionDefined) zipped
    mapM_ (uncurry throwIfVariableDefined) zipped
    let functions = map (\x -> over varMap (Map.insert x (ttype, lvl))) idents
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
  Class pos name@(Ident idname) classblock -> do
    throwIfClassDefined name pos
    lvl <- asks _nestLvl
    let self =
          over
            varMap
            (Map.insert (Ident "self") (T.TypeClass idname, lvl + 1))
    local (over nestLvl (+ 1) . self . set inClass (Just name)) $
      checkClassBlock classblock
    return (over classMap (Set.insert name))
  ClassInh pos name@(Ident idname) supername classblock -> do
    throwIfClassDefined name pos
    cycle <- detectInheritanceCycle idname
    gets (Map.lookup supername . _allClasses)
      >>= lookupFail (ClassNotInScope pos supername)
    when cycle $ throwError $ CyclicInheritance pos name
    (fields, methods) <- superFieldsMethods name
    lvl <- asks _nestLvl
    let fieldsF =
          foldr
            ((.) . \(i, t) -> over varMap (Map.insert i (t, lvl + 1)))
            id
            fields
    let methodsF =
          foldr ((.) . \i -> over funMap (Map.insert i (lvl + 1))) id methods

    let self =
          over
            varMap
            (Map.insert (Ident "self") (T.TypeClass idname, lvl + 2))
    local
      ( fieldsF . methodsF . self . over nestLvl (+ 2)
          . set
            inClass
            (Just name)
      )
      $ checkClassBlock classblock
    return (over classMap (Set.insert name))

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
    throwIfVariableDefined ident pos
    lvl <- asks _nestLvl
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
  Incr pos expr -> do
    throwIfWrongType expr T.TypeInt
    unless (isLvalue expr) $ throwError (AssignmentToRValue pos)
    return False
  Decr pos expr -> do
    checkStmt (Incr pos expr)
  Ret pos expr -> do
    rett <- asks _retType
    when (rett == T.Void) $ throwError (ReturnVoid pos)
    throwIfWrongType expr rett
    return True
  VRet pos -> do
    rett <- asks _retType
    unless (rett == T.Void) $ throwError $ TypeMismatch pos rett T.Void
    return True
  Cond pos expr stmt -> do
    checkStmt (CondElse pos expr stmt (Empty pos))
  CondElse p expr stmt1 stmt2 -> do
    throwIfWrongType expr T.TypeBool
    constE <- constExpr expr
    s1 <- checkBlock $ Block p [stmt1]
    s2 <- checkBlock $ Block p [stmt2]
    case constE of
      Just (ConstBool True) -> return s1
      Just (ConstBool False) -> return s2
      _ -> return $ s1 && s2
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
              T.TypeArray
                a
      _ -> throwError $ TypeMismatch pos exprt $ T.TypeArray typet
    lvl <- asks _nestLvl
    local
      (over varMap (Map.insert ident (typet, lvl + 1)) . over nestLvl (+ 1))
      (checkStmt stmt)
    return False
  SExp _ expr -> do
    checkExpr expr
    return False

checkScalarType :: Show a => ScalarType a -> SRE a T.Type
checkScalarType x = case x of
  ClassType pos ident@(Ident idident) -> do
    throwIfClassNotDefined ident pos
    return $ T.TypeClass idident
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
        throwIfClassNotDefined (Ident a) pos
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
  ELitInt _ _ -> do
    return T.TypeInt
  ELitTrue _ -> return T.TypeBool
  ELitFalse _ -> return T.TypeBool
  EString _ _ -> return T.TypeStr
  EApp pos ident exprs -> do
    a <- gets (Map.lookup ident . _allFuns)
    (ttype, args) <- case a of
      Just b -> return b
      Nothing -> do
        incls <- asks _inClass
        case incls of
          Just clsname -> do
            ClassMeta {_methods = methods} <-
              gets (Map.lookup clsname . _allClasses)
                >>= lookupFail (FunctionNotInScope pos ident)
            case Map.lookup ident methods of
              Just b -> return b
              Nothing -> throwError (FunctionNotInScope pos ident)
          Nothing -> throwError (FunctionNotInScope pos ident)
    throwIfArgumentsMismatch pos (map fst args) exprs
    return ttype
  EAccess pos indexed index -> do
    throwIfWrongType index T.TypeInt
    arrType <- checkExpr indexed
    case arrType of
      T.TypeArray a -> return a
      t -> throwError $ NonIndexable pos t
  ECast pos ident@(Ident idident) -> do
    ifM
      (gets (Map.member ident . _allClasses))
      (return $ T.TypeClass idident)
      (throwError $ SymbolNotDefined pos ident)
  Neg _ expr -> throwIfWrongType expr T.TypeInt
  Not _ expr -> throwIfWrongType expr T.TypeBool
  EMul _ expr1 _ expr2 -> throwIfWrong2Types T.TypeInt expr1 expr2
  EAdd pos expr1 op expr2 -> do
    expr1t <- checkExpr expr1
    expr2t <- checkExpr expr2
    unless (expr1t == expr2t) $ throwError (AddNonAddable pos expr1t expr2t)

    case op of
      (Plus _) ->
        unless (expr1t == T.TypeInt || expr1t == T.TypeStr) $
          throwError (AddNonAddable pos expr1t expr2t)
      (Minus _) ->
        unless (expr1t == T.TypeInt) $
          throwError (AddNonAddable pos expr1t expr2t)
    return expr1t
  ERel pos expr1 relop expr2 -> do
    expr1t <- checkExpr expr1
    expr2t <- checkExpr expr2
    unless (expr1t == expr2t) $
      throwError (CompareDifferentTypes pos expr1t expr2t)
    when (not (isEqNeq relop) && not (isComparable expr1t)) $
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

    isComparable T.TypeInt = True
    isComparable T.TypeStr = True
    isComparable _ = False

signatureType :: Show a => Type a -> T.Type
signatureType x = case x of
  NonVoidType _ nonvoidtype -> signatureNonVoidType nonvoidtype
  Void _ -> T.Void

signatureScalarType :: Show a => ScalarType a -> T.Type
signatureScalarType x = case x of
  ClassType _ (Ident ident) -> T.TypeClass ident
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
        (\(ClassField _ t idents) -> zip idents (repeat $ signatureType t))
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
