{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Types where

import AbsLatte (Ident)
import qualified Compiler.Backend.Tree as T
import Control.Lens hiding
  ( Empty,
    element,
  )
import Control.Lens.TH
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

data StaticStore = StaticStore
  { _allFuns :: FunMap,
    _allClasses :: ClassMap
  }

data StaticEnv = StaticEnv
  -- { _varMap :: VarMap -- used to track redefinition of symbol
  { _varMap :: VarMap, -- used to track redefinition of symbol
    _funMap :: Map.Map Ident NestLvl, -- used to track redefinition of symbol
    _classMap :: Set.Set Ident, -- used to track redefinition of symbol
    _retType :: RetType,
    _nestLvl :: NestLvl
  }

data ClassMeta = ClassMeta
  { _fields :: Map.Map Ident Field,
    _methods :: Map.Map Ident Function,
    _super :: Maybe Ident
  }

-- type ERT a r = ReaderT StaticEnv (Except (StaticException a)) r

type ERT a r =
  StateT StaticStore (ReaderT StaticEnv (Except (StaticException a))) r

type Function = (T.Type, [(T.Type, Ident)])

type Field = T.Type

type RetType = T.Type

type NestLvl = Integer

type VarMap = Map.Map Ident (T.Type, NestLvl)

type FunMap = Map.Map Ident Function

type ClassMap = Map.Map Ident ClassMeta

data StaticException a
  = VariableNotInScope a Ident
  | FunctionNotInScope a Ident
  | FieldNotInScope a T.Type Ident
  | MethodNotInScope a T.Type Ident
  | TypeNotDefined a T.Type
  | SymbolNotDefined a Ident
  | VoidField a
  | TypeMismatch a T.Type T.Type
  | CompareDifferentTypes a T.Type T.Type
  | NonIndexable a T.Type
  | RedefinitionOfSymbol a Ident
  | NoReturn a Ident
  | AddNonAddable a T.Type T.Type
  | WrongNumberOfArguments a Int Int
  | LogicOperationOnNonBooleans a T.Type T.Type
  | NonComparableTypes a T.Type T.Type
  | AssignmentToRValue a
  | NewOnNonClassType a T.Type
  | CyclicInheritance a Ident
  | MainNotDefined

instance Show (StaticException (Maybe (Int, Int))) where
  show (VariableNotInScope (Just (line, column)) ident) =
    positionString line column
      ++ "Variable "
      ++ show ident
      ++ " not in scope."
  show (FunctionNotInScope (Just (line, column)) ident) =
    positionString line column
      ++ "Function "
      ++ show ident
      ++ " not in scope."
  show (TypeMismatch (Just (line, column)) expected actual) =
    positionString line column
      ++ "Tried to use expresion of type "
      ++ show actual
      ++ " where expression of type "
      ++ show expected
      ++ " was expected."
  show (CompareDifferentTypes (Just (line, column)) t1 t2) =
    positionString line column
      ++ "Tried to compare expression of type "
      ++ show t1
      ++ " with expression of type "
      ++ show t2
      ++ "."
  show (NonIndexable (Just (line, column)) t) =
    positionString line column
      ++ "Tried to index expression of type "
      ++ show t
      ++ "."
  show (RedefinitionOfSymbol (Just (line, column)) name) =
    positionString line column
      ++ "Multiple declarations of variable "
      ++ show name
      ++ "."
  show (NoReturn (Just (line, column)) name) =
    positionString line column
      ++ "Function "
      ++ show name
      ++ " does not return in all execution paths."
  show (WrongNumberOfArguments (Just (line, column)) expected actual) =
    positionString line column
      ++ "Called function with wrong number of arguments, expected: "
      ++ show expected
      ++ ", provided: "
      ++ show actual
      ++ "."
  show MainNotDefined = "Error: main function not defined."
  show _ = "Udefined show"

positionString :: Int -> Int -> [Char]
positionString line column =
  "Error at line " ++ show line ++ " column " ++ show column ++ ":\n"

$(makeLenses ''StaticEnv)
