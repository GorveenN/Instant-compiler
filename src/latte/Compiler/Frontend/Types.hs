{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Frontend.Types where

import AbsLatte (Ident)
import qualified Compiler.Backend.Tree as T
import Control.Lens (makeLenses)
import Control.Monad.Except (Except)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import qualified Data.Map as Map
import qualified Data.Set as Set

data StaticStore = StaticStore
  { _allFuns :: FunMap,
    _allClasses :: ClassMap
  }
  deriving (Show)

data StaticEnv = StaticEnv
  -- { _varMap :: VarMap -- used to track redefinition of symbol
  { _varMap :: VarMap, -- used to track redefinition of symbol
    _funMap :: Map.Map Ident NestLvl, -- used to track redefinition of symbol
    _classMap :: Set.Set Ident, -- used to track redefinition of symbol
    _retType :: RetType,
    _nestLvl :: NestLvl
  }
  deriving (Show)

data ClassMeta = ClassMeta
  { _fields :: Map.Map Ident Field,
    _methods :: Map.Map Ident Function,
    _super :: Maybe Ident
  }
  deriving (Show)

-- type SRE a r = ReaderT StaticEnv (Except (StaticException a)) r

type SRE a r =
  StateT StaticStore (ReaderT StaticEnv (Except (StaticException a))) r

type Function = (T.Type, [(T.Type, Ident)])

type Field = T.Type

type RetType = T.Type

type NestLvl = Integer

type VarMap = Map.Map Ident (T.Type, NestLvl)

type FunMap = Map.Map Ident Function

type ClassMap = Map.Map Ident ClassMeta

data StaticException a
  = AddNonAddable a T.Type T.Type
  | AssignmentToRValue a
  | CompareDifferentTypes a T.Type T.Type
  | CyclicInheritance a Ident
  | FieldNotInScope a T.Type Ident
  | FunctionNotInScope a Ident
  | LogicOperationOnNonBooleans a T.Type T.Type
  | MainNotDefined
  | MethodNotInScope a T.Type Ident
  | NewOnNonClassType a T.Type
  | NoReturn a Ident
  | NonComparableTypes a T.Type T.Type
  | NonIndexable a T.Type
  | RedefinitionOfSymbol a Ident
  | SymbolNotDefined a Ident
  | TypeMismatch a T.Type T.Type
  | TypeNotDefined a T.Type
  | VariableNotInScope a Ident
  | VoidField a
  | WrongNumberOfArguments a Int Int

instance Show (StaticException (Maybe (Int, Int))) where
  show (AddNonAddable (Just (line, column)) t1 t2) =
    positionString line column
      ++ "Add non addable types: "
      ++ show t1
      ++ " and "
      ++ show t2
      ++ "."
  show (AssignmentToRValue (Just (line, column))) =
    positionString line column ++ "Assignment to rvalue."
  show (CompareDifferentTypes (Just (line, column)) t1 t2) =
    positionString line column
      ++ "Compare diffrent types : "
      ++ show t1
      ++ " and "
      ++ show t2
      ++ "."
  show (CyclicInheritance (Just (line, column)) i) =
    positionString line column
      ++ "Cycling inheritence including "
      ++ show i
      ++ "."
  show (FieldNotInScope (Just (line, column)) t i) =
    positionString line column
      ++ "Type "
      ++ show t
      ++ " does not have field named "
      ++ show i
      ++ "."
  show (FunctionNotInScope (Just (line, column)) i) =
    positionString line column ++ "Function " ++ show i ++ " not in scope."
  show (LogicOperationOnNonBooleans (Just (line, column)) t1 t2) =
    positionString line column
      ++ "Logic operation on "
      ++ show t1
      ++ " and "
      ++ show t2
      ++ "."
  show MainNotDefined = "Main function not defined."
  show (MethodNotInScope (Just (line, column)) t i) =
    positionString line column
      ++ "Type "
      ++ show t
      ++ " does not implement method "
      ++ show i
      ++ "."
  show (NewOnNonClassType (Just (line, column)) t) =
    positionString line column
      ++ "Call new on non class type "
      ++ show t
      ++ "."
  show (NoReturn (Just (line, column)) i) =
    positionString line column
      ++ "Function "
      ++ show i
      ++ " does not return in every execution path."
  show (NonComparableTypes (Just (line, column)) t1 t2) =
    positionString line column
      ++ show t1
      ++ " and "
      ++ show t2
      ++ " can not be compared."
  show (NonIndexable (Just (line, column)) t) =
    positionString line column ++ show t ++ "can not be indexed."
  show (RedefinitionOfSymbol (Just (line, column)) i) =
    positionString line column ++ "Redefinition of symbol " ++ show i ++ "."
  show (SymbolNotDefined (Just (line, column)) i) =
    positionString line column ++ "Symbol " ++ show i ++ " not defined."
  show (TypeMismatch (Just (line, column)) t1 t2) =
    positionString line column
      ++ "Used "
      ++ show t2
      ++ " where "
      ++ show t1
      ++ " was expected."
  show (TypeNotDefined (Just (line, column)) t) =
    positionString line column ++ "Type " ++ show t ++ " is not defined."
  show (VariableNotInScope (Just (line, column)) i) =
    positionString line column
      ++ "Variable "
      ++ show i
      ++ " is not defined."
  show (VoidField (Just (line, column))) =
    positionString line column ++ "Void class field."
  show (WrongNumberOfArguments (Just (line, column)) i1 i2) =
    positionString line column
      ++ "Expected "
      ++ show i1
      ++ " arguments "
      ++ " but "
      ++ show i2
      ++ " were/was provided."

-- show (TypeNotDefined {}) = "Type not defined"
-- show (VariableNotInScope (Just (line, column)) ident) =
--   positionString line column
--     ++ "Variable "
--     ++ show ident
--     ++ " not in scope."
-- show (FunctionNotInScope (Just (line, column)) ident) =
--   positionString line column
--     ++ "Function "
--     ++ show ident
--     ++ " not in scope."
-- show (TypeMismatch (Just (line, column)) expected actual) =
--   positionString line column
--     ++ "Tried to use expresion of type "
--     ++ show actual
--     ++ " where expression of type "
--     ++ show expected
--     ++ " was expected."
-- show (CompareDifferentTypes (Just (line, column)) t1 t2) =
--   positionString line column
--     ++ "Tried to compare expression of type "
--     ++ show t1
--     ++ " with expression of type "
--     ++ show t2
--     ++ "."
-- show (NonIndexable (Just (line, column)) t) =
--   positionString line column
--     ++ "Tried to index expression of type "
--     ++ show t
--     ++ "."
-- show (RedefinitionOfSymbol (Just (line, column)) name) =
--   positionString line column
--     ++ "Multiple declarations of variable "
--     ++ show name
--     ++ "."
-- show (NoReturn (Just (line, column)) name) =
--   positionString line column
--     ++ "Function "
--     ++ show name
--     ++ " does not return in all execution paths."
-- show (WrongNumberOfArguments (Just (line, column)) expected actual) =
--   positionString line column
--     ++ "Called function with wrong number of arguments, expected: "
--     ++ show expected
--     ++ ", provided: "
--     ++ show actual
--     ++ "."
-- show MainNotDefined = "Error: main function not defined."
-- show _ = "Udefined show"

positionString :: Int -> Int -> [Char]
positionString line column =
  "Error at line " ++ show line ++ " column " ++ show column ++ ":\n"

$(makeLenses ''StaticEnv)
