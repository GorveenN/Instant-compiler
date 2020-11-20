{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Frontend where

import           AbsLatte
import           ErrM

import           Compiler.Frontend.Types
import           Compiler.Frontend.Utils

import           Control.Monad                  ( foldM )
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Lens            hiding ( element
                                                , Empty
                                                )
import           Control.Lens.TH
import qualified Data.Map                      as Map

$(makeLenses ''StaticEnv)

lookupFail err = maybe (throwError err) return

type Result = Err String
failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x


-- TODO: pattern match may refuse when class is not present in env
superclass :: Show a => StaticType -> StaticType -> ERT a Bool
superclass _t1@(TypeCls t1) (TypeCls t2) = do
    maybeparent <- asks (Map.lookup t2 . _classMap)
    case maybeparent of
        (Just ClassMeta { _super = mparent }) -> do
            case mparent of
                (Just parent) -> if parent == t1
                    then return True
                    else superclass _t1 (TypeCls parent)
                Nothing -> return False
        Nothing -> return False

transProgram :: Show a => Program a -> Result
transProgram x = case x of
    Program _ topdefs -> failure x


transTopDef :: Show a => TopDef a -> Result
transTopDef x = case x of
    TopClassDef _ classdef -> failure x
    TopFunDef   _ fndef    -> failure x


checkFnDef :: Show a => FnDef a -> Result
checkFnDef x = case x of
    FnDef _ type_ ident args block -> failure x

checkIdentUnique :: Show a => [Arg a] -> ERT a [(Ident, StaticType)]
checkIdentUnique list = reverse <$> foldM foldFunc [] list
  where
    elemPair :: Eq a => a -> [(a, b)] -> Bool
    elemPair e = foldl (\acc x -> acc || (fst x == e)) False
    foldFunc
        :: Show a
        => [(Ident, StaticType)]
        -> Arg a
        -> ERT a [(Ident, StaticType)]
    foldFunc acc (Arg p t n) = if n `elemPair` acc
        then throwError (RedefinitionOfSymbol p n)
        else return $ (n, transType t) : acc

transArg :: Show a => Arg a -> Result
transArg x = case x of
    Arg _ type_ ident -> failure x


transClassMember :: Show a => ClassMember a -> Result
transClassMember x = case x of
    ClassField _ type_ idents -> failure x
    ClassMethod _ fndef       -> failure x


transClassBlock :: Show a => ClassBlock a -> ERT a ()
transClassBlock (ClassBlock p classmembers) = do
    let fields  = filter isField classmembers
    let methods = filter isMethod classmembers
    return ()

  where
    isField x = case x of
        (ClassMethod _ _) -> True
        _                 -> False
    isMethod = not . isField
    fieldMeta (ClassField _ _type _ident) = zip (repeat _type) _ident
      -- methodMeta (ClassMethod _ fndef)


transClassDef :: Show a => ClassDef a -> Result
transClassDef x = case x of
    Class _ ident classblock            -> failure x
    ClassInh _ ident1 ident2 classblock -> failure x


transBlock :: Show a => Block a -> Result
transBlock x = case x of
    Block _ stmts -> failure x


transStmt :: Show a => Stmt a -> ERT a Bool
transStmt x = case x of
    Empty _                 -> return False
    BStmt _ (Block p stmts) -> do
        local (over nestLvl (+ 1)) $ transStmtList stmts
    Decl _ type_ items -> do
        -- TODO
        return False
    Ass pos expr1 expr2 -> do
        expr1t <- transExpr expr1
        expr2t <- transExpr expr2
        unless (expr1t == expr2t) $ throwError $ TypeMismatch pos expr1t expr2t
        return False
    Incr pos ident -> do
        throwIfWrongType (EVar pos ident) TypeInt
        return False
    Decr pos ident -> transStmt (Incr pos ident)
    Ret  pos expr  -> do
        exprType     <- transExpr expr
        expectedType <- asks _retType
        when (exprType /= expectedType) $ throwError $ TypeMismatch
            pos
            expectedType
            exprType
        return True
    VRet pos -> do
        expectedType <- asks _retType
        unless (expectedType == TypeVoid) $ throwError $ TypeMismatch
            pos
            expectedType
            TypeVoid
        return False
    Cond _ expr stmt -> do
        throwIfWrongType expr TypeBool
        transStmt stmt
        return False
    CondElse _ expr stmt1 stmt2 -> do
        throwIfWrongType expr TypeBool
        liftM2 (&&) (transStmt stmt1) (transStmt stmt2)
    While _ expr stmt -> do
        throwIfWrongType expr TypeBool
        transStmt stmt
        return False
    For pos type_ ident expr stmt -> do
        let itemType = transType type_
        iterableType <- transExpr expr
        case iterableType of
            (TypeArr nestedType) -> do
                unless (nestedType == itemType) $ throwError $ TypeMismatch
                    pos
                    itemType
                    nestedType
                -- TODO Add ident to env
                lvl <- asks _nestLvl
                local
                    ( over nestLvl (+ 1)
                    . over varMap (Map.insert ident (iterableType, lvl + 1))
                    )
                    (transStmt stmt)
                transStmt stmt
                return False
            _ -> throwError $ TypeMismatch pos (TypeArr itemType) iterableType
    SExp _ expr -> do
        transExpr expr
        return False

transStmtList :: Show a => [Stmt a] -> ERT a Bool
transStmtList ((Decl pos type_ items) : rest) = do
    return False
transStmtList (s : ss) = liftM2 (||) (transStmt s) (transStmtList ss)
transStmtList []       = False


transItem :: Show a => Item a -> ERT a ()
transItem x = case x of
    NoInit _ ident    -> return ()
    Init _ ident expr -> do
        exprType <- transExpr expr
        return ()


transClassType :: Show a => ClassType a -> StaticType
transClassType x = case x of
    BCType _ ident -> TypeCls ident


transBType :: Show a => BType a -> StaticType
transBType x = case x of
    Int  _ -> TypeInt
    Str  _ -> TypeStr
    Bool _ -> TypeBool


transNonVoidType :: Show a => NonVoidType a -> StaticType
transNonVoidType x = case x of
    CType _ (BCType _ ident) -> TypeCls ident
    BType _ btype            -> transBType btype


transType :: Show a => Type a -> StaticType
transType x = case x of
    ArrayType   _ nonvoidtype -> transNonVoidType nonvoidtype
    NonVoidType _ nonvoidtype -> transNonVoidType nonvoidtype
    Void _                    -> TypeVoid


transCastType :: Show a => CastType a -> StaticType
transCastType x = case x of
    CastTypeClass _ nonvoidtype -> transNonVoidType nonvoidtype
    CastTypeArr   _ nonvoidtype -> transNonVoidType nonvoidtype


throwArgumentsMismatch :: Show a => a -> [StaticType] -> [Expr a] -> ERT a ()
throwArgumentsMismatch pos formal actual = do
    when (length formal /= length actual) $ throwError $ WrongNumberOfArguments
        pos
        (length formal)
        (length actual)
    zipWithM_
        (\t expr -> do
            exprType <- transExpr expr
            when (exprType /= t)
                $ throwError (TypeMismatch (exprPosition expr) t exprType)
        )
        formal
        actual


transExpr :: Show a => Expr a -> ERT a StaticType
transExpr x = case x of
    ENewObject pos ident -> do
        let t = TypeCls ident
        throwIfNotDef pos t
        return t
    ENewArray pos nonvoidtype expr -> do
        throwIfWrongType expr TypeInt
        let arrType = transNonVoidType nonvoidtype
        throwIfNotDef pos arrType
        return arrType
    EMember p expr ident -> do
        objType <- transExpr expr
        fieldType p objType ident
        return TypeBool
    EMemberCall pos expr ident arguments -> do
        objType <- transExpr expr
        throwIfNotDef pos objType
        (returnType, argumentsTypes) <- methodType pos objType ident
        throwArgumentsMismatch pos (map fst argumentsTypes) arguments
        return returnType
    EVar pos ident -> do
        fst
            <$> (   asks (Map.lookup ident . _varMap)
                >>= lookupFail (VariableNotInScope pos ident)
                )
    ELitInt _ _              -> return TypeInt
    ELitTrue  _              -> return TypeBool
    ELitFalse _              -> return TypeBool
    ENull     _              -> return TypeNull
    EString _ _              -> return TypeStr
    EApp pos ident arguments -> do
        (returnType, argumentsTypes) <- asks (Map.lookup ident . _funMap)
            >>= lookupFail (FunctionNotInScope pos ident)
        throwArgumentsMismatch pos (map fst argumentsTypes) arguments
        return returnType
    EAccess pos toIndex index -> do
        toIndexType <- transExpr toIndex
        case toIndexType of
            TypeArr _ -> throwIfWrongType index TypeInt
            _         -> throwError $ NonIndexable pos toIndexType
    ECast p expr cast -> do
        exprType <- transExpr expr
        let castType = transCastType cast
        unless (exprType == TypeNull) $ throwError $ TypeMismatch p
                                                                  TypeNull
                                                                  exprType
        return castType
    Neg _ expr           -> throwIfWrongType expr TypeInt
    Not _ expr           -> throwIfWrongType expr TypeBool
    EMul _ lhs mulop rhs -> do
        throwIfWrongType lhs TypeInt
        throwIfWrongType rhs TypeInt
    EAdd pos lhs addop rhs -> do
        lhsType <- transExpr lhs
        rhsType <- transExpr rhs
        unless (lhsType == rhsType) $ throwError $ TypeMismatch pos
                                                                lhsType
                                                                rhsType
        return lhsType
    ERel p lhs relop rhs -> do
        lhsType <- transExpr lhs
        rhsType <- transExpr rhs
        when (lhsType /= rhsType) $ throwError $ CompareDifferentTypes
            p
            lhsType
            rhsType
        return TypeBool
    EAnd p lhs rhs -> do
        throwIfWrongType lhs TypeBool
        throwIfWrongType rhs TypeBool
        return TypeBool
    EOr p lhs rhs -> transExpr $ EAnd p lhs rhs



throwIfWrongType :: Show a => Expr a -> StaticType -> ERT a StaticType
throwIfWrongType expr ttype = do
    t <- transExpr expr
    if t == ttype
        then return ttype
        else throwError $ TypeMismatch (exprPosition expr) ttype t

throwIfNotDef :: Show a => a -> StaticType -> ERT a ()
throwIfNotDef p t = do
    defined <- typeDefined t
    unless defined $ throwError (TypeNotDefined p t)

sigFnDef :: Show a => FnDef a -> (Ident, Function)
sigFnDef (FnDef _ type_ ident args _) = (ident, (stype, argsTypes))
  where
    stype     = transType type_
    argsTypes = sigArgList args

sigClassBlock
    :: Show a => ClassBlock a -> ([(Ident, Field)], [(Ident, Function)])
sigClassBlock (ClassBlock _ classmembers) = (fields, methods)
  where
    isField x = case x of
        (ClassMethod _ _) -> True
        _                 -> False
    isMethod = not . isField
    fieldTrans (ClassField _ _type _ident) =
        zip _ident (repeat $ transType _type)
    methodTrans (ClassMethod _ fndef) = sigFnDef fndef
    fields  = concatMap fieldTrans $ filter isField classmembers
    methods = map methodTrans $ filter isMethod classmembers


sigArgList :: Show a => [Arg a] -> [(StaticType, Ident)]
sigArgList = map (\(Arg _ t n) -> (transType t, n))


fieldType :: Show a => a -> StaticType -> Ident -> ERT a StaticType
fieldType p (TypeCls className) fieldName = do
    ClassMeta { _fields = fieldsMap } <- asks (Map.lookup className . _classMap)
        >>= lookupFail (ClassNotInScope p className)
    case Map.lookup fieldName fieldsMap of
        (Just t) -> return t
        _        -> throwError $ MemberNotFound p (TypeCls className) fieldName
fieldType p className fieldName =
    throwError $ MemberNotFound p className fieldName


methodType :: Show a => a -> StaticType -> Ident -> ERT a Function
methodType p (TypeCls className) methodName = do
    ClassMeta { _methods = methodsMap } <-
        asks (Map.lookup className . _classMap)
            >>= lookupFail (ClassNotInScope p className)
    case Map.lookup methodName methodsMap of
        (Just t) -> return t
        _        -> throwError $ MemberNotFound p (TypeCls className) methodName
methodType p className methodName =
    throwError $ MemberNotFound p className methodName

typeDefined :: Show a => StaticType -> ERT a Bool
typeDefined (TypeArr t) = typeDefined t
typeDefined (TypeCls n) = asks $ Map.member n . _classMap
typeDefined _           = return True
