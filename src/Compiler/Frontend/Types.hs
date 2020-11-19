{-# LANGUAGE FlexibleInstances #-}
module Compiler.Frontend.Types where

import           AbsLatte

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map


data StaticEnv = StaticEnv
    { _varMap :: VarMap
    , _funMap :: FunMap
    , _classMap :: ClassMap
    , _retType :: RetType
    , _nestLvl :: NestLvl
    , _className :: Maybe Ident
    }

data ClassMeta = ClassMeta
    { _fields :: Map.Map Ident Field
    , _methods :: Map.Map Ident Function
    , _super :: Maybe Ident
    }

type ERT a r = ReaderT StaticEnv (Except (StaticException a)) r

type Function = (StaticType, [(StaticType, Ident)])
type Field = StaticType
type RetType = StaticType

type NestLvl = Integer
type VarMap = Map.Map Ident (StaticType, NestLvl)
type FunMap = Map.Map Ident Function
type ClassMap = Map.Map Ident ClassMeta

data StaticType
    = TypeBool
    | TypeVoid
    | TypeInt
    | TypeStr
    | TypeNull
    | TypeCls Ident
    | TypeArr StaticType
    deriving (Ord)

instance Show StaticType where
    show TypeVoid        = "bool"
    show TypeBool        = "bool"
    show TypeInt         = "int"
    show TypeStr         = "string"
    show (TypeArr inner) = "[" ++ show inner ++ "]"

instance Eq StaticType where
    TypeInt    == TypeInt    = True
    TypeStr    == TypeStr    = True
    TypeBool   == TypeBool   = True
    TypeCls n1 == TypeCls n2 = n1 == n2
    TypeArr t1 == TypeArr t2 = t1 == t2
    _          == _          = False


data StaticException a = ArrayNotHomogenous a
    | VariableNotInScope a Ident
    | FunctionNotInScope a Ident
    | ClassNotInScope  a Ident
    | TypeMismatch a StaticType StaticType
    | TypeNotDefined a StaticType
    | CompareDifferentTypes a StaticType StaticType
    | AddNonAddable a
    | NonIndexable a StaticType
    | RedefinitionOfSymbol a Ident
    | NoReturn a Ident
    | WrongNumberOfArguments a Int Int
    | NotSuperclass a Ident Ident
    | MemberNotFound a StaticType Ident
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
    show (AddNonAddable (Just (line, column))) =
        positionString line column ++ "Tried to add two boolean values"
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
