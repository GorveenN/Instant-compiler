module AbsLatte where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq, Ord, Read)

instance Show Ident where
  show (Ident s) = s

data Program a = Program a [TopDef a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
  fmap f x = case x of
    Program a topdefs -> Program (f a) (map (fmap f) topdefs)

data TopDef a = TopClassDef a (ClassDef a) | TopFunDef a (FnDef a)
  deriving (Eq, Ord, Show, Read)

instance Functor TopDef where
  fmap f x = case x of
    TopClassDef a classdef -> TopClassDef (f a) (fmap f classdef)
    TopFunDef a fndef -> TopFunDef (f a) (fmap f fndef)

data FnDef a = FnDef a (Type a) Ident [Arg a] (Block a)
  deriving (Eq, Ord, Show, Read)

instance Functor FnDef where
  fmap f x = case x of
    FnDef a type_ ident args block -> FnDef (f a) (fmap f type_) ident (map (fmap f) args) (fmap f block)

data Arg a = Arg a (NonVoidType a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Arg where
  fmap f x = case x of
    Arg a nonvoidtype ident -> Arg (f a) (fmap f nonvoidtype) ident

data ClassMember a
  = ClassField a (Type a) [Ident]
  | ClassMethod a (FnDef a)
  deriving (Eq, Ord, Show, Read)

instance Functor ClassMember where
  fmap f x = case x of
    ClassField a type_ idents -> ClassField (f a) (fmap f type_) idents
    ClassMethod a fndef -> ClassMethod (f a) (fmap f fndef)

data ClassBlock a = ClassBlock a [ClassMember a]
  deriving (Eq, Ord, Show, Read)

instance Functor ClassBlock where
  fmap f x = case x of
    ClassBlock a classmembers -> ClassBlock (f a) (map (fmap f) classmembers)

data ClassDef a
  = Class a Ident (ClassBlock a)
  | ClassInh a Ident Ident (ClassBlock a)
  deriving (Eq, Ord, Show, Read)

instance Functor ClassDef where
  fmap f x = case x of
    Class a ident classblock -> Class (f a) ident (fmap f classblock)
    ClassInh a ident1 ident2 classblock -> ClassInh (f a) ident1 ident2 (fmap f classblock)

data Block a = Block a [Stmt a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
  fmap f x = case x of
    Block a stmts -> Block (f a) (map (fmap f) stmts)

data Stmt a
  = Empty a
  | BStmt a (Block a)
  | Decl a (NonVoidType a) [Item a]
  | Ass a (Expr a) (Expr a)
  | Incr a Ident
  | Decr a Ident
  | Ret a (Expr a)
  | VRet a
  | Cond a (Expr a) (Stmt a)
  | CondElse a (Expr a) (Stmt a) (Stmt a)
  | While a (Expr a) (Stmt a)
  | For a (NonVoidType a) Ident (Expr a) (Stmt a)
  | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Stmt where
  fmap f x = case x of
    Empty a -> Empty (f a)
    BStmt a block -> BStmt (f a) (fmap f block)
    Decl a nonvoidtype items -> Decl (f a) (fmap f nonvoidtype) (map (fmap f) items)
    Ass a expr1 expr2 -> Ass (f a) (fmap f expr1) (fmap f expr2)
    Incr a ident -> Incr (f a) ident
    Decr a ident -> Decr (f a) ident
    Ret a expr -> Ret (f a) (fmap f expr)
    VRet a -> VRet (f a)
    Cond a expr stmt -> Cond (f a) (fmap f expr) (fmap f stmt)
    CondElse a expr stmt1 stmt2 -> CondElse (f a) (fmap f expr) (fmap f stmt1) (fmap f stmt2)
    While a expr stmt -> While (f a) (fmap f expr) (fmap f stmt)
    For a nonvoidtype ident expr stmt -> For (f a) (fmap f nonvoidtype) ident (fmap f expr) (fmap f stmt)
    SExp a expr -> SExp (f a) (fmap f expr)

data Item a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Item where
  fmap f x = case x of
    NoInit a ident -> NoInit (f a) ident
    Init a ident expr -> Init (f a) ident (fmap f expr)

data ScalarType a = ClassType a Ident | Int a | Str a | Bool a
  deriving (Eq, Ord, Show, Read)

instance Functor ScalarType where
  fmap f x = case x of
    ClassType a ident -> ClassType (f a) ident
    Int a -> Int (f a)
    Str a -> Str (f a)
    Bool a -> Bool (f a)

data NonVoidType a
  = ArrayType a (ScalarType a)
  | ScalarType a (ScalarType a)
  deriving (Eq, Ord, Show, Read)

instance Functor NonVoidType where
  fmap f x = case x of
    ArrayType a scalartype -> ArrayType (f a) (fmap f scalartype)
    ScalarType a scalartype -> ScalarType (f a) (fmap f scalartype)

data Type a = NonVoidType a (NonVoidType a) | Void a
  deriving (Eq, Ord, Show, Read)

instance Functor Type where
  fmap f x = case x of
    NonVoidType a nonvoidtype -> NonVoidType (f a) (fmap f nonvoidtype)
    Void a -> Void (f a)

data Expr a
  = ENewObject a (ScalarType a)
  | ENewArray a (ScalarType a) (Expr a)
  | EField a (Expr a) Ident
  | EMethodCall a (Expr a) Ident [Expr a]
  | EVar a Ident
  | ELitInt a Integer
  | ELitTrue a
  | ELitFalse a
  | EString a String
  | EApp a Ident [Expr a]
  | EAccess a (Expr a) (Expr a)
  | ECast a Ident
  | Neg a (Expr a)
  | Not a (Expr a)
  | EMul a (Expr a) (MulOp a) (Expr a)
  | EAdd a (Expr a) (AddOp a) (Expr a)
  | ERel a (Expr a) (RelOp a) (Expr a)
  | EAnd a (Expr a) (Expr a)
  | EOr a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
  fmap f x = case x of
    ENewObject a scalartype -> ENewObject (f a) (fmap f scalartype)
    ENewArray a scalartype expr -> ENewArray (f a) (fmap f scalartype) (fmap f expr)
    EField a expr ident -> EField (f a) (fmap f expr) ident
    EMethodCall a expr ident exprs -> EMethodCall (f a) (fmap f expr) ident (map (fmap f) exprs)
    EVar a ident -> EVar (f a) ident
    ELitInt a integer -> ELitInt (f a) integer
    ELitTrue a -> ELitTrue (f a)
    ELitFalse a -> ELitFalse (f a)
    EString a string -> EString (f a) string
    EApp a ident exprs -> EApp (f a) ident (map (fmap f) exprs)
    EAccess a expr1 expr2 -> EAccess (f a) (fmap f expr1) (fmap f expr2)
    ECast a ident -> ECast (f a) ident
    Neg a expr -> Neg (f a) (fmap f expr)
    Not a expr -> Not (f a) (fmap f expr)
    EMul a expr1 mulop expr2 -> EMul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
    EAdd a expr1 addop expr2 -> EAdd (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
    ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
    EAnd a expr1 expr2 -> EAnd (f a) (fmap f expr1) (fmap f expr2)
    EOr a expr1 expr2 -> EOr (f a) (fmap f expr1) (fmap f expr2)

data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
  fmap f x = case x of
    Plus a -> Plus (f a)
    Minus a -> Minus (f a)

data MulOp a = Times a | Div a | Mod a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
  fmap f x = case x of
    Times a -> Times (f a)
    Div a -> Div (f a)
    Mod a -> Mod (f a)

data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
  fmap f x = case x of
    LTH a -> LTH (f a)
    LE a -> LE (f a)
    GTH a -> GTH (f a)
    GE a -> GE (f a)
    EQU a -> EQU (f a)
    NE a -> NE (f a)
