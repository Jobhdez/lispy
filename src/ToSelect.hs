module ToSelect where

import Parser
import ToAnf
import ToCir

data Immediate = Imm Int
data Register = Reg String
data MemoryRef = Ref String

data Argument =
  ArgImm Immediate
  | ArgReg Register
  | ArgMem MemoryRef

data Instruction =
  Addq Argument Argument
  | Subq Argument Argument
  | Negq Argument
  | Movq Argument Argument
  | Pushq Argument
  | Popq Argument
  | Jmp String
  | Callq String Int
  | Retq String
  | Cmpq Argument Argument
  | Incq Argument
