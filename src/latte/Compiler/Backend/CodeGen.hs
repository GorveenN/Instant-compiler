module Compiler.Backend.CodeGen where

import Control.Lens (makeLenses)
import Control.Monad.Reader
  ( ReaderT,
    runReaderT,
  )
import Control.Monad.State
  ( StateT,
    evalStateT,
  )
import Control.Monad.Writer
  ( Writer,
    WriterT,
    runWriter,
    tell,
  )
import Data.List (intercalate)
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
  deriving (Eq)

instance Show Register where
  show EAX = "%eax"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EBX = "%ebx"
  show ESP = "%esp"
  show EBP = "%ebp"
  show ESI = "%esi"
  show EDI = "%edi"

eax_ :: Operand
eax_ = Register EAX

ecx_ :: Operand
ecx_ = Register ECX

esp_ :: Operand
esp_ = Register ESP

ebp_ :: Operand
ebp_ = Register EBP

edx_ :: Operand
edx_ = Register EDX

ebx_ :: Operand
ebx_ = Register EBX

type Offset = Integer

type Label = String

data Operand
  = Register Register
  | Memory Register (Maybe Offset)
  | Const Integer
  | Label Label
  deriving (Eq)

instance Show Operand where
  show (Register r) = show r
  show (Memory r (Just o)) = show o ++ "(" ++ show r ++ ")"
  show (Memory r Nothing) = "(" ++ show r ++ ")"
  show (Const i) = "$" ++ show i
  show (Label l) = "$" ++ l

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
  | CALL Label
  | NEG Operand
  | INC Operand
  | DEC Operand
  | RET

instance Show Instruction where
  show (IDIV o) = "idiv " ++ show o
  show CDQ = "cdq"
  show (IMUL o1 o2) = "imul " ++ show o1 ++ ", " ++ show o2
  show (MOV o1 o2) = "mov " ++ show o1 ++ ", " ++ show o2
  show (ADD o1 o2) = "add " ++ show o1 ++ ", " ++ show o2
  show (SUB o1 o2) = "sub " ++ show o1 ++ ", " ++ show o2
  show (POP o) = "pop " ++ show o
  show (CMP o1 o2) = "cmp " ++ show o1 ++ ", " ++ show o2
  show (PUSH o) = "push " ++ show o
  show (JMP l) = "jmp " ++ l
  show (JE l) = "je " ++ l
  show (JG l) = "jg " ++ l
  show (JGE l) = "jge " ++ l
  show (JL l) = "jl " ++ l
  show (JLE l) = "jle " ++ l
  show (JNE l) = "jne " ++ l
  show (LABEL l) = l ++ ":"
  show (CALL l) = "call " ++ l
  show (NEG l) = "neg " ++ show l
  show (INC l) = "incl " ++ show l
  show (DEC l) = "decl " ++ show l
  show RET = "ret"

data VTable = VTable Label [Label]

instance Show VTable where
  show (VTable classname methods) =
    classname ++ "::VTable: .int " ++ intercalate ", " methods

type SRW s e d =
  StateT s (ReaderT e (Writer ([Instruction], [StringLiteral], [VTable]))) d

data StringLiteral = StringLiteral String Label

instance Show StringLiteral where
  show (StringLiteral s l) = l ++ ":\n" ++ ".string \"" ++ s ++ "\""

tellInstruction :: Instruction -> SRW s e ()
tellInstruction xs = tell ([xs], mempty, mempty)

tellStringLiteral :: StringLiteral -> SRW s e ()
tellStringLiteral xs = tell (mempty, [xs], mempty)

tellVTable :: VTable -> SRW s e ()
tellVTable xs = tell (mempty, mempty, [xs])

add_ :: Operand -> Operand -> SRW s e ()
add_ l r = tellInstruction $ ADD l r

sub_ :: Operand -> Operand -> SRW s e ()
sub_ l r = tellInstruction $ SUB l r

imul_ :: Operand -> Operand -> SRW s e ()
imul_ l r = tellInstruction $ IMUL l r

idiv_ :: Operand -> SRW s e ()
idiv_ l = tellInstruction $ IDIV l

push_ :: Operand -> SRW s e ()
push_ l = tellInstruction $ PUSH l

pop_ :: Operand -> SRW s e ()
pop_ l = tellInstruction $ POP l

mov_ :: Operand -> Operand -> SRW s e ()
mov_ l r = tellInstruction $ MOV l r

cdq_ :: SRW s e ()
cdq_ = tellInstruction CDQ

cmp_ :: Operand -> Operand -> SRW s e ()
cmp_ l r = tellInstruction $ CMP l r

jmp_ :: Label -> SRW s e ()
jmp_ l = tellInstruction $ JMP l

jg_ :: Label -> SRW s e ()
jg_ l = tellInstruction $ JG l

jge_ :: Label -> SRW s e ()
jge_ l = tellInstruction $ JGE l

jl_ :: Label -> SRW s e ()
jl_ l = tellInstruction $ JL l

jle_ :: Label -> SRW s e ()
jle_ l = tellInstruction $ JLE l

jne_ :: Label -> SRW s e ()
jne_ l = tellInstruction $ JNE l

je_ :: Label -> SRW s e ()
je_ l = tellInstruction $ JE l

label_ :: Label -> SRW s e ()
label_ l = tellInstruction $ LABEL l

neg_ :: Operand -> SRW s e ()
neg_ l = tellInstruction $ NEG l

ret_ :: SRW s e ()
ret_ = tellInstruction RET

inc_ :: Operand -> SRW s e ()
inc_ l = tellInstruction $ INC l

dec_ :: Operand -> SRW s e ()
dec_ l = tellInstruction $ DEC l

call_ :: Label -> SRW s e ()
call_ n = tellInstruction $ CALL n
