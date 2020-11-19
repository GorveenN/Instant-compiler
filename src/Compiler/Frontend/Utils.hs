module Compiler.Frontend.Utils where

import           AbsLatte
import Compiler.Frontend.Types

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map

exprPosition :: Expr a -> a
exprPosition expr = case expr of
    EVar    pos _         -> pos
    ELitInt pos _         -> pos
    ELitTrue  pos         -> pos
    ELitFalse pos         -> pos
    EApp pos _ _          -> pos
    EString pos _         -> pos
    EAccess pos _ _       -> pos
    Neg pos _             -> pos
    Not pos _             -> pos
    EMul pos _ _ _        -> pos
    EAdd pos _ _ _        -> pos
    ERel pos _ _ _        -> pos
    EAnd pos _ _          -> pos
    EOr  pos _ _          -> pos
    ENewObject pos _      -> pos
    ENewArray pos _ _     -> pos
    EMember   pos _ _     -> pos
    EMemberCall pos _ _ _ -> pos
    ENull pos             -> pos
    ECast pos _ _         -> pos

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
