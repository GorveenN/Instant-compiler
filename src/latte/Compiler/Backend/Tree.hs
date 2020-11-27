module Compiler.Backend.Tree where

import AbsLatte (Ident)

data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = ClassDef | FunDef [FnDef]
  deriving (Eq, Ord, Show, Read)

data FnDef = FnDef Type Ident [TypedIdent] Stmt
  deriving (Eq, Ord, Show, Read)

data TypedIdent = TypedIdent Type Ident
  deriving (Eq, Ord, Show, Read)

data ClassBlock = ClassBlock [TypedIdent] [FnDef]
  deriving (Eq, Ord, Show, Read)

data ClassDef
  = Class Ident ClassBlock
  | ClassInh Ident Ident ClassBlock
  deriving (Eq, Ord, Show, Read)

data Stmt
  = Block [Stmt]
  | Decl Type Item
  | Ass Expr Expr
  | Incr Ident
  | Decr Ident
  | Ret Expr
  | VRet
  | Cond Expr Stmt
  | CondElse Expr Stmt Stmt
  | While Expr Stmt
  | For Type Ident Expr Stmt
  | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type = TypeInt | TypeBool | TypeStr | TypeClass Ident | TypeArray Type | Void
  deriving (Eq, Ord, Read)

data Expr
  = ENewObject Type
  | ENewArray Type Expr
  | EField Expr Ident
  | EMethodCall Expr Ident [Expr]
  | EVar Ident
  | ELitInt Integer
  | ELitTrue
  | ELitFalse
  | EString String
  | EApp Ident [Expr]
  | EAccess Expr Expr
  | ECast Type
  | Neg Expr
  | Not Expr
  | EArithm Expr AritmOp Expr
  | ELogic Expr LogicOp Expr
  deriving (Eq, Ord, Show, Read)

data AritmOp = Plus | Minus | Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data LogicOp = LTH | LE | GTH | GE | EQU | NE | AND | OR
  deriving (Eq, Ord, Show, Read)

instance Show Type where
  show TypeBool = "bool"
  show TypeInt = "int"
  show TypeStr = "string"
  show (TypeClass n) = "class " ++ show n
  show (TypeArray n) = show n ++ " []"
  show Void = "void"
