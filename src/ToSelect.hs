module ToSelect where

import Parser
import Desugar
import ToAnf
import ToCir
import qualified Data.Map as Map

-- this module is the instruction selection module. It takes the Cir AST
-- and selects the x86 instructions.

data Argument =
    Immediate Int
  | Register String
  | MemoryRef String
  deriving Show

data Instruction =
    Addq Argument Argument
  | Subq Argument Argument
  | Negq Argument
  | Movq Argument Argument
  | Pushq Argument
  | Popq Argument
  | Jmp String
  | Je String
  | Jl String
  | Callq String
  | Retq
  | Cmpq Argument Argument
  | Incq Argument
  | Setg Argument
  | Setl Argument
  | Sete Argument
  | Movzbq Argument Argument
  | Label String
  | Xorq Argument Argument
  deriving Show

toselect :: ([Cir], Map.Map String [Cir]) -> [Instruction]
toselect ([], _) = []
toselect ((x:xs), blocks) =
  case x of
    Assign (CVar var) (CInt n) ->
      Movq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
      
    CPlus (CVar var) (CVar var') ->
      Movq (MemoryRef var) (Register "%rax") : Addq (Register "%rax") (MemoryRef var') : toselect (xs, blocks)

    CPlus (CVar var) (CInt e) ->
      [Addq (Immediate e) (MemoryRef var)]

    CPlus (CInt e) (CVar var) ->
      [Addq (Immediate e) (MemoryRef var)]

    CPlus (CInt e) (CInt e2) ->
      Movq (Immediate e) (Register "%rax") : Addq (Immediate e2) (Register "%rax") : toselect (xs, blocks)

    CMinus (CVar var) (CVar var') ->
      Movq (MemoryRef var) (Register "%rax") : Subq (Register "%rax") (MemoryRef var') : toselect (xs, blocks)

    CMinus (CVar var) (CInt e) ->
      [Subq (Immediate e) (MemoryRef var)]

    CMinus (CInt e) (CVar var) ->
      [Subq (Immediate e) (MemoryRef var)]

    CMinus (CInt e) (CInt e2) ->
      Movq (Immediate e) (Register "%rax") : Subq (Immediate e2) (Register "%rax") : toselect (xs, blocks)

    CLess (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) : toselect (xs, blocks)

    CLess (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var) : toselect (xs, blocks)

    CLess (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e)  : toselect (xs, blocks)

    CLess (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e)  : toselect (xs, blocks)
---
    CGreater (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) :  toselect (xs, blocks)

    CGreater (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var) : toselect (xs, blocks)

    CGreater (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e) : toselect (xs, blocks)

    CGreater (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e)  : toselect (xs, blocks)
---
    CEq (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) :  toselect (xs, blocks)

    CEq (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var)  : toselect (xs, blocks)

    CEq (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e) : toselect (xs, blocks)

    CEq (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e) : toselect (xs, blocks)
    
    CEq (CVar var) (CBool e) ->
      let bool = if e == True then 1 else 0 in
        Cmpq (Immediate bool) (MemoryRef var) : toselect (xs, blocks)

    CEq (CBool e) (CVar var) ->
      let bool = if e == True then 1 else 0 in
        Cmpq (MemoryRef var) (Immediate bool) : toselect (xs, blocks)

    CEq (CBool e) (CBool b) ->
      let bool1 = if e == True then 1 else 0 in
        let bool2 = if b == True then 1 else 0 in
          Cmpq (Immediate bool2) (Immediate bool1) : toselect (xs, blocks)
      
    -- todo: cminus  
    Assign (CVar var) (CLess (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setl (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CLess (CInt a) (CVar b)) ->
       Cmpq (MemoryRef b) (Immediate a) : Setl (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CLess (CVar a) (CInt b)) ->
       Cmpq (Immediate b) (MemoryRef a) : Setl (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CLess (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Setl (Register "%al")): (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks)
      
  ---
    Assign (CVar var) (CGreater (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setg (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CGreater (CInt a) (CVar b)) ->
       Cmpq (MemoryRef b) (Immediate a) : Setg (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CGreater (CVar a) (CInt b)) ->
       Cmpq (Immediate b) (MemoryRef a) : Setg (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CGreater (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Setg (Register "%al")): (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks)

    Assign (CVar var) (CPlus (CInt a) (CInt b)) ->
       Movq (Immediate a) (MemoryRef var) : Addq (Immediate b) (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CPlus (CInt a) (CVar b)) ->
      Movq (Immediate a) (Register "%rax") : Addq (Register "%rax") (MemoryRef b) : toselect (xs, blocks)

    Assign (CVar var) (CPlus (CVar b) (CInt a)) ->
      Movq (Immediate a) (Register "%rax") : Addq (Register "%rax") (MemoryRef b) : toselect (xs, blocks)

    Assign (CVar var) (CPlus (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : Addq (Register "%rax") (MemoryRef a): toselect (xs, blocks)

    --

    Assign (CVar var) (CMinus (CInt a) (CInt b)) ->
       Movq (Immediate a) (MemoryRef var) : Subq (Immediate b) (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CMinus (CInt a) (CVar b)) ->
      Movq (Immediate a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks)

    Assign (CVar var) (CMinus (CVar b) (CInt a)) ->
      Movq (Immediate a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks)

    Assign (CVar var) (CMinus (CVar a) (CVar b)) ->
      Movq (MemoryRef a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks)
    
      
    Assign (CVar var) (CEq (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Sete (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Sete (Register "%al")) : (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks)

    Assign (CVar var) (CEq (CVar a) (CInt b)) ->
      Cmpq (Immediate b) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CInt b) (CVar a)) ->
      Cmpq (Immediate b) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CBool b) (CVar a)) ->
      let bool = if b == True then 1 else 0 in
        Cmpq (MemoryRef a) (Immediate bool) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CVar a) (CBool b)) ->
      let bool = if b == True then 1 else 0 in
        Cmpq (Immediate bool) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CBool a) (CBool b)) ->
      let bool1 = if a == True then 1 else 0 in
        let bool2 = if b == True then 1 else 0 in
          Cmpq (Immediate bool2) (Immediate bool1) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks)
                                                                     
    Assign (CVar var) (CNot n) ->

      case n of
        CVar nvar ->
          Xorq  (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        CInt i ->
          Movq (Immediate i) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        CBool True ->
          Movq (Immediate 1) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        _ -> Movq (Immediate 0) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks) 
     
    
    IfGoto (CVar var) (Goto block) (Goto block') ->
      let blk = Map.lookup block blocks
          blk' = Map.lookup block' blocks
      in case (blk, blk') of
          (Just blkInstrs, Just blk'Instrs) ->
            Cmpq (Immediate 1) (MemoryRef var) : Je block : Jmp block' : Label block : toselect (blkInstrs, blocks) ++  [Label block'] ++ toselect (blk'Instrs, blocks) ++ toselect (xs, blocks)
          _ -> error "Block not found"
          
    CInt n -> Movq (Immediate n) (Register "%rax") : Callq "print_int" : toselect (xs, blocks)

    CVar var -> Movq (MemoryRef var) (Register "%rdi") : Callq "print_int" : toselect (xs, blocks)

    CBool b ->
      if b == True
      then Movq (Immediate 1) (Register "%rax") : toselect (xs, blocks)
      else
        Movq (Immediate 0) (Register "%rax") : toselect (xs, blocks)
    
    CBegin exps -> concatMap (\cirexp -> toselect ([cirexp], blocks)) exps ++ toselect (xs, blocks)
    
    IfGotoLoop cnd (Goto block) ->
      let blk = Map.lookup block blocks
      in case blk of
           (Just blk') ->
             Label (block) : toselect (blk', blocks) ++ toselect ([cnd], blocks) ++ [Jl block] ++ toselect (xs, blocks)
           _ -> error "Block not found"
    _ -> error "Unsupported CIR instruction"
