{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Frontend.Utils where

import AbsLatte
import qualified Compiler.Backend.Tree as T
import {-# SOURCE #-} Compiler.Frontend.Frontend
import Compiler.Frontend.Types
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

lookupFail err = maybe (throwError err) return

memberFail _ True = return False
memberFail err False = throwError err

exprPosition :: Expr a -> a
exprPosition expr = case expr of
  EVar pos _ -> pos
  ELitInt pos _ -> pos
  ELitTrue pos -> pos
  ELitFalse pos -> pos
  EApp pos _ _ -> pos
  EString pos _ -> pos
  EAccess pos _ _ -> pos
  Neg pos _ -> pos
  Not pos _ -> pos
  EMul pos _ _ _ -> pos
  EAdd pos _ _ _ -> pos
  ERel pos _ _ _ -> pos
  EAnd pos _ _ -> pos
  EOr pos _ _ -> pos
  ENewObject pos _ -> pos
  ENewArray pos _ _ -> pos
  EField pos _ _ -> pos
  EMethodCall pos _ _ _ -> pos
  ECast pos _ -> pos

checkIdentUnique :: Show a => [(Ident, a)] -> SRE a ()
checkIdentUnique list = do
  foldM_ foldFunc [] list
  where
    elemPair e = foldl (\acc x -> acc || (fst x == e)) False
    foldFunc acc (n, p) =
      if n `elemPair` acc
        then throwError (RedefinitionOfSymbol p n)
        else return $ (n, p) : acc

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
throwIfVariableDefined name pos = do
  a <- asks $ Map.lookup name . _varMap
  case a of
    (Just (_, vlvl)) -> do
      lvl <- asks _nestLvl
      when (lvl == vlvl) $ throwError (RedefinitionOfSymbol pos name)
    _ -> return ()

throwIfVariableNotDefined :: Show a => Ident -> a -> SRE a ()
throwIfVariableNotDefined = _throwSymbol _varMap _throwNothing

throwIfFunctionDefined :: Show a => Ident -> a -> SRE a ()
throwIfFunctionDefined name pos = do
  a <- asks $ Map.lookup name . _funMap
  case a of
    (Just vlvl) -> do
      lvl <- asks _nestLvl
      when (lvl == vlvl) $ throwError (RedefinitionOfSymbol pos name)
    _ -> return ()

throwIfClassDefined :: Show a => Ident -> a -> SRE a ()
throwIfClassDefined var pos = do
  a <- asks $ Set.member var . _classMap
  when a $ throwError $ RedefinitionOfSymbol pos var

throwIfClassNotDefined :: Show a => Ident -> a -> SRE a ()
throwIfClassNotDefined var pos = do
  a <- gets $ Map.member var . _allClasses
  unless a $ throwError $ TypeNotDefined pos $ T.TypeClass var

isLvalue :: Expr a -> Bool
isLvalue EField {} = True
isLvalue EVar {} = True
isLvalue EAccess {} = True
isLvalue _ = False

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

detectInheritanceCycle :: Show a => Ident -> SRE a Bool
detectInheritanceCycle name = superclass (T.TypeClass name) (T.TypeClass name)