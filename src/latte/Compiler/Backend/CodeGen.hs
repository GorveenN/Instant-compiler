module Compiler.Backend.CodeGen where

-- import Compiler.Backend.Backend (emmitExpr)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer
  ( Writer,
    WriterT,
    runWriter,
    tell,
  )

data Register = EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI deriving (Show)

eax = EAX

type Offset = Integer

data Operand
  = Register Register (Maybe Offset)
  | Memory Register (Maybe Offset)
  | Const Integer
  deriving (Show)

-- data Instruction =
data Instruction = Idiv Operand | Cdq | Imul Operand Operand | Mov Operand Operand | Add Operand Operand | Sub Operand Operand | Pop Operand | Push Operand deriving (Show)

type SRW s e d = StateT s (ReaderT e (Writer [Instruction])) d

type CodeGen d = SRW Integer Integer d

add_ :: Operand -> Operand -> CodeGen ()
add_ l r = tell [Add l r]

sub_ :: Operand -> Operand -> CodeGen ()
sub_ l r = tell [Sub l r]

imul_ :: Operand -> Operand -> CodeGen ()
imul_ l r = tell [Imul l r]

idiv_ :: Operand -> CodeGen ()
idiv_ l = tell [Idiv l]

push_ :: Operand -> CodeGen ()
push_ l = tell [Push l]

pop_ :: Operand -> CodeGen ()
pop_ l = tell [Pop l]

mov_ :: Operand -> Operand -> CodeGen ()
mov_ l r = tell [Mov l r]

cdq_ :: CodeGen ()
cdq_ = tell [Cdq]
