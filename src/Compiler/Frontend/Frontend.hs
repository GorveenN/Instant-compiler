{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Compiler.Frontend.Frontend where

-- import           AbsLatte
-- import           ErrM

import           Compiler.Frontend.Types
import           Compiler.Frontend.Utils

import           Control.Monad                  ( foldM )
import           Control.Monad.Extra            ( concatMapM )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Lens            hiding ( element
                                                , Empty
                                                )
import           Control.Lens.TH
import qualified Data.Map                      as Map

import qualified Compiler.Backend.Tree         as T

import           AbsLatte
import           ErrM
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
    TopFunDef   _ fndef    -> checkFnDef fndef


checkFnDef :: Show a => FnDef a -> ERT a (StaticEnv -> StaticEnv)
checkFnDef x = case x of
    FnDef _ type_ ident_ args_ block_ -> do
        args  <- checkArgs args_
        rtype <- checkType type_
        lvl   <- asks _nestLvl
        let argF = foldr ((.) . (\(t, i) -> Map.insert i (t, lvl))) id args
        local (over nestLvl (+ 1) . over varMap argF) (checkBlock block_)
        let funF = Map.insert ident_ ((rtype, args), lvl)
        return (over funMap funF)

checkArgs :: Show a => [Arg a] -> ERT a [(T.Type, Ident)]
checkArgs args = do
    argst <- mapM checkArg args
    let idents = map (snd . fst) argst
    checkIdentUnique $ zip idents (map snd argst)
    return $ map fst argst

checkArg :: Show a => Arg a -> ERT a ((T.Type, Ident), a)
checkArg (Arg pos nonvoidtype ident) = do
    t <- checkNonVoidType nonvoidtype
    return ((t, ident), pos)


-- TODO
detectInheritanceCycle :: Ident -> ERT a Bool
detectInheritanceCycle name = return True

checkClassMember :: Show a => ClassMember a -> ERT a (StaticEnv -> StaticEnv)
checkClassMember x = case x of
    ClassField pos type_ idents -> do
        ttype <- checkType type_
        let zipped = zip idents $ repeat pos
        lvl <- asks _nestLvl
        checkIdentUnique zipped
        mapM_ (uncurry throwIfMethodDefined)   zipped
        mapM_ (uncurry throwIfVariableDefined) zipped
        let functions =
                map (\x -> over varMap (Map.insert x (ttype, lvl))) idents
        return $ foldr (.) id functions
    ClassMethod _ fndef -> checkFnDef fndef


checkClassBlock :: Show a => ClassBlock a -> ERT a (StaticEnv -> StaticEnv)
checkClassBlock x = case x of
    ClassBlock _ classmembers -> do
        let fields  = filter isField classmembers
        let methods = filter isField classmembers
        checkList checkClassMember $ fields ++ methods
        return id

checkClassDef :: Show a => ClassDef a -> ERT a (StaticEnv -> StaticEnv)
checkClassDef x = case x of
    Class _ name classblock -> do
        checkClassBlock classblock
        return (over classMap (Map.insert name Nothing))
    ClassInh pos name supername classblock -> do
        checkClassBlock classblock
        cycle <- detectInheritanceCycle name
        when cycle $ throwError $ CyclicInheritance pos name
        return (over classMap (Map.insert name (Just supername)))


isField x = case x of
    (ClassMethod _ _) -> True
    _                 -> False
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
checkStmtList (decl@(Decl{}) : ss) = checkStmtDecl decl ss
checkStmtList (s : ss) = liftM2 (||) (checkStmt s) (checkStmtList ss)
checkStmtList [] = return False

-- TODO
superclass :: T.Type -> T.Type -> Bool
superclass t1 t2 = False

checkStmt :: Show a => Stmt a -> ERT a Bool
checkStmt x = case x of
    Empty _             -> return False
    BStmt _ block       -> checkBlock block
    Ass pos expr1 expr2 -> do
        expr1t <- checkExpr expr1
        expr2t <- checkExpr expr2
        unless (expr1t == expr2t || expr2t `superclass` expr1t)
            $ throwError
            $ TypeMismatch pos expr1t expr2t
        return False
    Incr pos ident -> do
        throwIfWrongType (EVar pos ident) T.TypeInt
        throwIfVariableNotDefined ident pos
        return False
    Decr pos ident -> do
        checkStmt (Incr pos ident)
    Ret _ expr                        -> return False
    VRet _                            -> return False
    Cond _ expr stmt                  -> return False
    CondElse _ expr stmt1 stmt2       -> return False
    While _ expr stmt                 -> return False
    For _ nonvoidtype ident expr stmt -> return False
    SExp _ expr                       -> return False



checkScalarType :: Show a => ScalarType a -> ERT a T.Type
checkScalarType x = case x of
    ClassType _ ident -> failure x
    Int  _            -> failure x
    Str  _            -> failure x
    Bool _            -> failure x


checkNonVoidType :: Show a => NonVoidType a -> ERT a T.Type
checkNonVoidType x = case x of
    ArrayType  _ scalartype -> failure x
    ScalarType _ scalartype -> failure x


checkType :: Show a => Type a -> ERT a T.Type
checkType x = case x of
    NonVoidType _ nonvoidtype -> failure x
    Void _                    -> failure x


checkExpr :: Show a => Expr a -> ERT a T.Type
checkExpr x = case x of
    ENewObject pos nonvoidtype -> do
        let t = checkNonVoidType nonvoidtype
        case t of
            T.TypeClass _ -> do
                throwIfNotDef pos t
                return t
            _ -> throwError $ NewOnNonClassType pos t
        return t
    ENewArray pos indexed index -> do
        -- indexed is arrType
        throwIfWrongType index T.TypeInt
        arrType <- checkExpr indexed
        throwIfNotDef pos arrType
        return possiblyArrType
    EMember _ expr ident           -> failure x
    EMemberCall _ expr ident exprs -> failure x
    EVar    _ ident                -> failure x
    ELitInt _ integer              -> failure x
    ELitTrue  _                    -> failure x
    ELitFalse _                    -> failure x
    ENull     _                    -> failure x
    EString _ string               -> failure x
    EApp    _ ident exprs          -> failure x
    EAccess _ expr1 expr2          -> failure x
    ECast _ ident                  -> failure x
    Neg   _ expr                   -> failure x
    Not   _ expr                   -> failure x
    EMul _ expr1 mulop expr2       -> failure x
    EAdd _ expr1 addop expr2       -> failure x
    ERel _ expr1 relop expr2       -> failure x
    EAnd _ expr1 expr2             -> failure x
    EOr  _ expr1 expr2             -> failure x


checkAddOp :: Show a => AddOp a -> Result
checkAddOp x = case x of
    Plus  _ -> failure x
    Minus _ -> failure x


checkMulOp :: Show a => MulOp a -> Result
checkMulOp x = case x of
    Times _ -> failure x
    Div   _ -> failure x
    Mod   _ -> failure x


checkRelOp :: Show a => RelOp a -> Result
checkRelOp x = case x of
    LTH _ -> failure x
    LE  _ -> failure x
    GTH _ -> failure x
    GE  _ -> failure x
    EQU _ -> failure x
    NE  _ -> failure x

getSuperMembers :: Ident -> [ClassMeta]
getSuperMembers = undefined

throwIfWrongType :: Show a => Expr a -> T.Type -> ERT a T.Type
throwIfWrongType expr ttype = do
    e@(_t, _) <- checkExpr expr
    let t = _t
    if t == ttype
        then return e
        else throwError $ TypeMismatch (exprPosition expr) ttype t

throwIfTypeNotDefined :: Show a => a -> T.Type -> ERT a ()
throwIfTypeNotDefined p t = do
    defined <- typeDefined t
    unless defined $ throwError (TypeNotDefined p t)

throwIfSymbolNotDefined :: Show a => a -> Ident -> ERT a ()
throwIfSymbolNotDefined = undefined

throwIfArgumentsMismatch
    :: Show a => a -> [T.Type] -> [Expr a] -> ERT a [T.Type]
throwIfArgumentsMismatch pos formal actual = do
    when (length formal /= length actual) $ throwError $ WrongNumberOfArguments
        pos
        (length formal)
        (length actual)
    zipWithM
        (\t expr -> do
            exprType <- checkExpr expr
            when (exprType /= t)
                $ throwError (TypeMismatch (exprPosition expr) t exprType)
            return exprType
        )
        formal
        actual

_throwIfSymbolNotDefined f var pos = do
    a <- asks $ Map.lookup var . f
    case a of
        Nothing -> throwError $ SymbolNotDefined pos var
        _       -> return ()

throwIfVariableNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfVariableNotDefined = _throwIfSymbolNotDefined _varMap

throwIfMethodNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfMethodNotDefined = _throwIfSymbolNotDefined _funMap

throwIfClassNotDefined :: Show a => Ident -> a -> ERT a ()
throwIfClassNotDefined var pos = do
    a <- asks $ Map.lookup var . _classMap
    case a of
        Just _  -> throwError $ RedefinitionOfSymbol pos var
        Nothing -> return ()


signatureFunction :: Show a => FnDef a -> ERT a (Ident, Function)
signatureFunction = undefined

checkIdentUnique :: Show a => [(Ident, a)] -> ERT a ()
checkIdentUnique list = do
    foldM_ foldFunc [] list
  where
    elemPair e = foldl (\acc x -> acc || (fst x == e)) False
    foldFunc acc (n, p) = if n `elemPair` acc
        then throwError (RedefinitionOfSymbol p n)
        else return $ (n, p) : acc


-- checkClassBlock
--     :: Show a => ClassBlock a -> ERT a ([(Ident, Field)], [(Ident, Function)])
-- checkClassBlock (ClassBlock _ ((ClassField pos t idents) : rest)) = do
--     return ([], [])
-- checkClassBlock
--     :: Show a => ClassBlock a -> ERT a ([(Ident, Field)], [(Ident, Function)])
-- checkClassBlock (ClassBlock _ classmembers) = do
--     checkIdentUnique allSymbols
--     fields  <- concatMapM fieldTrans $ filter isField classmembers
--     methods <- mapM methodTrans $ filter isMethod classmembers
--     return (fields, methods)
--   where
--     isField x = case x of
--         (ClassMethod _ _) -> True
--         _                 -> False
--     isMethod = not . isField
--     fieldTrans (ClassField _ (NonVoidType _ _type) _ident) = do
--         return $ zip _ident (repeat $ checkNonVoidType _type)
--     fieldTrans (ClassField _ (Void p) _ident) = do
--         throwError $ VoidField p
--     methodTrans (ClassMethod _ fndef) = signatureFunction fndef
--     allSymbols = foldl
--         (\acc x -> case x of
--             (ClassMethod p (FnDef _ _ n _ _)) -> (n, p) : acc
--             (ClassField p _ names           ) -> zip names (repeat p) ++ acc
--         )
--         []
--         classmembers

-- signatureClassBlock
--     :: Show a => ClassBlock a -> ERT a ([(Ident, Field)], [(Ident, Function)])
-- signatureClassBlock (ClassBlock _ classmembers) = do
--     checkIdentUnique allSymbols
--     fields  <- concatMapM fieldTrans $ filter isField classmembers
--     methods <- mapM methodTrans $ filter isMethod classmembers
--     return (fields, methods)
--   where
--     isField x = case x of
--         (ClassMethod _ _) -> True
--         _                 -> False
--     isMethod = not . isField
--     fieldTrans (ClassField _ (NonVoidType _ _type) _ident) = do
--         return $ zip _ident (repeat $ checkNonVoidType _type)
--     fieldTrans (ClassField _ (Void p) _ident) = do
--         throwError $ VoidField p
--     methodTrans (ClassMethod _ fndef) = signatureFunction fndef
--     allSymbols = foldl
--         (\acc x -> case x of
--             (ClassMethod p (FnDef _ _ n _ _)) -> (n, p) : acc
--             (ClassField p _ names) -> zip names (repeat p) ++ acc
--         )
--         []
--         classmembers


getMethodType :: Show a => a -> T.Type -> Ident -> ERT a Function
getMethodType pos cls@(T.TypeClass className) methodName = do
    ClassMeta { _methods = methodsMap } <-
        asks (Map.lookup className . _classMap)
            >>= lookupFail (TypeNotDefined pos cls)
    case Map.lookup methodName methodsMap of
        (Just t) -> return t
        _        -> throwError $ MethodNotInScope pos cls methodName
methodType pos className methodName =
    throwError $ MethodNotInScope pos className methodName

getFieldType :: Show a => a -> T.Type -> Ident -> ERT a T.Type
getFieldType pos cls@(T.TypeClass className) fieldName = do
    ClassMeta { _fields = fieldsMap } <- asks (Map.lookup className . _classMap)
        >>= lookupFail (TypeNotDefined pos cls)
    case Map.lookup fieldName fieldsMap of
        (Just t) -> return t
        _        -> throwError $ FieldNotInScope pos cls fieldName
fieldType pos className fieldName =
    throwError $ FieldNotInScope pos className fieldName

typeDefined :: Show a => T.Type -> ERT a Bool
typeDefined (T.TypeArray t) = typeDefined t
typeDefined (T.TypeClass n) = asks $ Map.member n . _classMap
typeDefined _               = return True

lookupFail err = maybe (throwError err) return
