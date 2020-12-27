{-# LANGUAGE TemplateHaskell #-}

module Compiler.Backend.CodeGen where

-- import Compiler.Backend.Backend (emmitExpr)

import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Writer
  ( Writer,
    WriterT,
    runWriter,
    tell,
  )
import Data.Map

data Register
  = EAX
  | ECX
  | EDX
  | EBX
  | ESP
  | EBP
  | ESI
  | EDI
  deriving (Show, Eq)

eax_ = Register EAX Nothing

esp_ = Register ESP Nothing

ebp_ = Register EBP Nothing

type Offset = Integer

type Label = String

data Operand
  = Register Register (Maybe Offset)
  | Memory Register (Maybe Offset)
  | Const Integer
  deriving (Show, Eq)

data Instruction
  = IDIV Operand
  | CDQ
  | IMUL Operand Operand
  | MOV Operand Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | POP Operand
  | CMP Operand Operand
  | PUSH Operand
  | JMP Label
  | JE Label
  | JG Label
  | JGE Label
  | JL Label
  | JLE Label
  | JNE Label
  | LABEL Label
  | NEG Operand
  | INC Operand
  | DEC Operand
  | RET
  deriving (Show)

type SRW s e d = StateT s (ReaderT e (Writer [Instruction])) d

type CodeGen d = SRW Integer Store d

data Store = Store
  { _vars :: Map String Integer,
    _stackH :: Integer
  }

$(makeLenses ''Store)

add_ :: Operand -> Operand -> CodeGen ()
add_ l r = tell [ADD l r]

sub_ :: Operand -> Operand -> CodeGen ()
sub_ l r = tell [SUB l r]

imul_ :: Operand -> Operand -> CodeGen ()
imul_ l r = tell [IMUL l r]

idiv_ :: Operand -> CodeGen ()
idiv_ l = tell [IDIV l]

push_ :: Operand -> CodeGen ()
push_ l = tell [PUSH l]

pop_ :: Operand -> CodeGen ()
pop_ l = tell [POP l]

mov_ :: Operand -> Operand -> CodeGen ()
mov_ l r = tell [MOV l r]

cdq_ :: CodeGen ()
cdq_ = tell [CDQ]

cmp_ :: Operand -> Operand -> CodeGen ()
cmp_ l r = tell [CMP l r]

jmp_ :: Label -> CodeGen ()
jmp_ l = tell [JMP l]

jg_ :: Label -> CodeGen ()
jg_ l = tell [JG l]

jge_ :: Label -> CodeGen ()
jge_ l = tell [JGE l]

jl_ :: Label -> CodeGen ()
jl_ l = tell [JL l]

jle_ :: Label -> CodeGen ()
jle_ l = tell [JLE l]

jne_ :: Label -> CodeGen ()
jne_ l = tell [JNE l]

je_ :: Label -> CodeGen ()
je_ l = tell [JE l]

label_ :: Label -> CodeGen ()
label_ l = tell [LABEL l]

neg_ :: Operand -> CodeGen ()
neg_ l = tell [NEG l]

ret_ :: CodeGen ()
ret_ = tell [RET]

inc_ :: Operand -> CodeGen ()
inc_ l = tell [INC l]

dec_ :: Operand -> CodeGen ()
dec_ l = tell [DEC l]
