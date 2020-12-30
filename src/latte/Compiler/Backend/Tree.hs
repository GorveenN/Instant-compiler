module Compiler.Backend.Tree where

import qualified AbsLatte as A

type Id = String

data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = TopClassDef ClassDef | TopFnDef FnDef
  deriving (Eq, Ord, Show, Read)

data FnDef = FnDef Type Id [TypedId] Stmt
  deriving (Eq, Ord, Show, Read)

data TypedId = TypedId Type Id
  deriving (Eq, Ord, Show, Read)

data ClassBlock = ClassBlock [TypedId] [FnDef]
  deriving (Eq, Ord, Show, Read)

data ClassDef
  = Class Id ClassBlock
  | ClassInh Id Id ClassBlock
  deriving (Eq, Ord, Show, Read)

data Stmt
  = Block [Stmt]
  | Decl Type Id Expr
  | Ass Expr Expr
  | Incr Expr
  | Decr Expr
  | Ret Expr
  | VRet
  | Cond Expr Stmt
  | CondElse Expr Stmt Stmt
  | While Expr Stmt
  | For Type Id Expr Stmt
  | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Type
  = TypeInt
  | TypeBool
  | TypeStr
  | TypeClass Id
  | TypeArray Type
  | Void
  deriving (Eq, Ord, Read)

data Expr
  = ENewObject Type
  | ENewArray Type Expr
  | EField Expr Id
  | EMethodCall Expr Id [Expr]
  | EVar Id
  | ELitInt Integer
  | EString String
  | EApp Id [Expr]
  | EAccess Expr Expr
  | Neg Expr
  | Not Expr
  | EArithm Expr AritmOp Expr
  | ELogic Expr LogicOp Expr
  deriving (Eq, Ord, Show, Read)

data AritmOp
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Eq, Ord, Show, Read)

data LogicOp
  = LTH
  | LE
  | GTH
  | GE
  | EQU
  | NE
  | AND
  | OR
  deriving (Eq, Ord, Show, Read)

instance Show Type where
  show TypeBool = "bool"
  show TypeInt = "int"
  show TypeStr = "string"
  show (TypeClass n) = "class " ++ show n
  show (TypeArray n) = show n ++ " []"
  show Void = "void"

transIdent :: A.Ident -> Id
transIdent (A.Ident a) = a

transProgram :: Show a => A.Program a -> Program
transProgram (A.Program _ s) = Program $ map transTopDef s

transTopDef :: Show a => A.TopDef a -> TopDef
transTopDef (A.TopClassDef _ cls) = TopClassDef $ transClassDef cls
transTopDef (A.TopFunDef _ fn) = TopFnDef $ transFnDef fn

transFnDef :: Show a => A.FnDef a -> FnDef
transFnDef (A.FnDef _ type_ name args body@(A.Block _ ss)) =
  FnDef
    (transType type_)
    (transIdent name)
    (map transArg args)
    (transBlock retBody)
  where
    retBody = case (type_, reverse ss) of
      (A.Void _, A.VRet _ : _) -> body
      (A.Void _, _) -> addRet body
      _ -> body
    addRet (A.Block a ss) = A.Block a (ss ++ [A.VRet a])

transArg :: Show a => A.Arg a -> TypedId
transArg (A.Arg _ type_ name) =
  TypedId (transNonVoidType type_) (transIdent name)

transClassDef :: Show a => A.ClassDef a -> ClassDef
transClassDef (A.Class _ name block) =
  Class (transIdent name) (transClassBlock block)
transClassDef (A.ClassInh _ name inh block) =
  ClassInh (transIdent name) (transIdent inh) (transClassBlock block)

transClassBlock :: Show a => A.ClassBlock a -> ClassBlock
transClassBlock (A.ClassBlock _ ss) = ClassBlock (getFields ss) (getMethods ss)
  where
    getFields ((A.ClassField _ type_ idents) : rest) =
      map (TypedId (transType type_) . transIdent) idents ++ getFields rest
    getFields (_ : rest) = getFields rest
    getFields [] = []
    getMethods ((A.ClassMethod _ fndef) : rest) =
      transFnDef fndef : getMethods rest
    getMethods (_ : rest) = getMethods rest
    getMethods [] = []

transBlock :: Show a => A.Block a -> Stmt
transBlock (A.Block _ ss) = Block (concatMap transStmt ss)

transStmtToBlock (A.BStmt _ block) = transBlock block
transStmtToBlock a = Block $ transStmt a

transStmt :: Show a => A.Stmt a -> [Stmt]
transStmt (A.Empty _) = []
transStmt (A.BStmt _ block) = [transBlock block]
transStmt (A.Decl _ type_ items) =
  map (transItem (transNonVoidType type_)) items
transStmt (A.Ass _ e1 e2) = [Ass (transExpr e1) (transExpr e2)]
transStmt (A.Incr _ e) = [Incr $ transExpr e]
transStmt (A.Decr _ e) = [Decr $ transExpr e]
transStmt (A.Ret _ e) = [Ret $ transExpr e]
transStmt (A.VRet _) = [VRet]
transStmt (A.Cond _ e s) = case e' of
  (ELitInt 1) -> [s']
  (ELitInt 0) -> []
  _ -> [Cond (transExpr e) (transStmtToBlock s)]
  where
    e' = transExpr e
    s' = transStmtToBlock s
transStmt (A.CondElse _ e s1 s2) = case e' of
  (ELitInt 1) -> [s1']
  (ELitInt 0) -> [s2']
  _ -> [CondElse e' s1' s2']
  where
    e' = transExpr e
    s1' = transStmtToBlock s1
    s2' = transStmtToBlock s2
transStmt (A.While _ e s) = [While (transExpr e) (transStmtToBlock s)]
transStmt (A.For _ type_ name e s) =
  [ For
      (transNonVoidType type_)
      (transIdent name)
      (transExpr e)
      (transStmtToBlock s)
  ]
transStmt (A.SExp _ e) = [SExp $ transExpr e]

transItem :: Show a => Type -> A.Item a -> Stmt
transItem t (A.NoInit _ ident) = Decl t (transIdent ident) e
  where
    e = case t of
      TypeInt -> ELitInt 0
      TypeBool -> ELitInt 0
      TypeStr -> EString ""
      TypeArray _ -> ELitInt 0
      TypeClass _ -> ELitInt 0
transItem t (A.Init _ ident e) = Decl t (transIdent ident) (transExpr e)

transScalarType :: Show a => A.ScalarType a -> Type
transScalarType (A.ClassType _ ident) = TypeClass $ transIdent ident
transScalarType (A.Int _) = TypeInt
transScalarType (A.Str _) = TypeStr
transScalarType (A.Bool _) = TypeBool

transNonVoidType :: Show a => A.NonVoidType a -> Type
transNonVoidType (A.ArrayType _ type_) = TypeArray $ transScalarType type_
transNonVoidType (A.ScalarType _ type_) = transScalarType type_

transType :: Show a => A.Type a -> Type
transType (A.NonVoidType _ type_) = transNonVoidType type_
transType (A.Void _) = Void

transExpr :: Show a => A.Expr a -> Expr
transExpr (A.ENewObject _ t) = ENewObject $ transScalarType t
transExpr (A.ENewArray _ t e) = ENewArray (transScalarType t) (transExpr e)
transExpr (A.EField _ e i) = EField (transExpr e) (transIdent i)
transExpr (A.EMethodCall _ e i exprs) =
  EMethodCall (transExpr e) (transIdent i) (map transExpr exprs)
transExpr (A.EVar _ i) = EVar $ transIdent i
transExpr (A.ELitInt _ i) = ELitInt i
transExpr (A.ELitTrue _) = ELitInt 1
transExpr (A.ELitFalse _) = ELitInt 0
transExpr (A.EString _ s) = EString (tail $ init s)
transExpr (A.EApp _ i exprs) = EApp (transIdent i) (map transExpr exprs)
transExpr (A.EAccess _ e1 e2) = EAccess (transExpr e1) (transExpr e2)
transExpr (A.Neg _ e) = case e' of
  (ELitInt i) -> ELitInt (- i)
  _ -> Neg e'
  where
    e' = transExpr e
transExpr (A.Not _ e) = Not $ transExpr e
transExpr (A.EMul _ e1 op e2) = simplifyAddMul transMulOp fop e1 op e2
  where
    fop x = case x of
      Times -> (*)
      Div -> div
      Mod -> rem
transExpr (A.EAdd _ e1 op e2) = simplifyAddMul transAddOp fop e1 op e2
  where
    fop x = case x of
      Plus -> (+)
      Minus -> (-)
transExpr (A.ERel _ e1 op e2) = case (e1', e2') of
  (ELitInt i1, ELitInt i2) -> boolToExpr $ fop e1' e2'
  (EString i1, EString i2) -> boolToExpr $ fop e1' e2'
  _ -> ELogic e1' op' e2'
  where
    e1' = transExpr e1
    e2' = transExpr e2
    op' = transRelOp op
    fop = case op' of
      LTH -> (<=)
      LE -> (<)
      GTH -> (>=)
      GE -> (>)
      EQU -> (==)
      NE -> (/=)
    boolToExpr :: Bool -> Expr
    boolToExpr False = ELitInt 0
    boolToExpr True = ELitInt 1
transExpr (A.EAnd _ e1 e2) = case (e1', e2') of
  (ELitInt 1, other) -> other
  (ELitInt 0, _) -> ELitInt 0
  (other, ELitInt 1) -> other
  (_, ELitInt 0) -> ELitInt 0
  _ -> ELogic e1' AND e2'
  where
    e1' = transExpr e1
    e2' = transExpr e2
transExpr (A.EOr _ e1 e2) = case (e1', e2') of
  (ELitInt 0, other) -> other
  (ELitInt 1, _) -> ELitInt 1
  (other, ELitInt 0) -> other
  (_, ELitInt 1) -> ELitInt 1
  _ -> ELogic e1' OR e2'
  where
    e1' = transExpr e1
    e2' = transExpr e2
transExpr (A.ECast _ _) = ELitInt 0

simplifyAddMul f1 f2 e1 op e2 = case (e1', e2') of
  (ELitInt i1, ELitInt i2) -> ELitInt (fop i1 i2)
  _ -> EArithm e1' op' e2'
  where
    op' = f1 op
    fop = f2 op'
    e1' = transExpr e1
    e2' = transExpr e2

transAddOp :: Show a => A.AddOp a -> AritmOp
transAddOp (A.Plus _) = Plus
transAddOp (A.Minus _) = Minus

transMulOp :: Show a => A.MulOp a -> AritmOp
transMulOp (A.Times _) = Times
transMulOp (A.Div _) = Div
transMulOp (A.Mod _) = Mod

transRelOp :: Show a => A.RelOp a -> LogicOp
transRelOp (A.LTH a) = LTH
transRelOp (A.LE a) = LE
transRelOp (A.GTH a) = GTH
transRelOp (A.GE a) = GE
transRelOp (A.EQU a) = EQU
transRelOp (A.NE a) = NE
