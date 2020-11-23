{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.Frontend.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map

import qualified Compiler.Backend.Tree         as T


import           Control.Lens            hiding ( element
                                                , Empty
                                                )
import           Control.Lens.TH

import           AbsLatte                       ( Ident )


data StaticEnv = StaticEnv
    -- { _varMap :: VarMap -- used to track redefinition of symbol
    { _varMap :: VarMap -- used to track redefinition of symbol
    , _funMap :: FunMap -- used to track redefinition of symbol
    , _classMap :: ClassMap -- used to track redefinition of symbol
    , _allVars :: VarMap
    , _allFuns :: FunMap
    , _allClasses :: ClassMap
    , _retType :: RetType
    , _nestLvl :: NestLvl
    }

data ClassMeta = ClassMeta
    { _fields :: Map.Map Ident Field
    , _methods :: Map.Map Ident Function
    , _super :: Maybe Ident
    }

type ERT a r = ReaderT StaticEnv (Except (StaticException a)) r

type Function = (T.Type, [(T.Type, Ident)])
type Field = T.Type
type RetType = Maybe T.Type

type NestLvl = Integer
type VarMap = Map.Map Ident (T.Type, NestLvl)
type FunMap = Map.Map Ident (Function, NestLvl)
type ClassMap = Map.Map Ident ClassMeta

data StaticException a
    = VariableNotInScope a Ident
    | FunctionNotInScope a Ident
    | FieldNotInScope a T.Type Ident
    | MethodNotInScope a T.Type Ident
    | TypeNotDefined a T.Type
    | VoidField a
    | TypeMismatch a T.Type T.Type
    | CompareDifferentTypes a T.Type T.Type
    | NonIndexable a T.Type
    | RedefinitionOfSymbol a Ident
    | NoReturn a Ident
    | WrongNumberOfArguments a Int Int
    | LogicOperationOnNonBooleans a T.Type T.Type
    | NewOnNonClassType a T.Type
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
    show _              = "Udefined show"

positionString :: Int -> Int -> [Char]
positionString line column =
    "Error at line " ++ show line ++ " column " ++ show column ++ ":\n"

$(makeLenses ''StaticEnv)
