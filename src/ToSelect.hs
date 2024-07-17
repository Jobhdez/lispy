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
  | Callq String Int
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
      Addq (MemoryRef var) (MemoryRef var') : toselect (xs, blocks)
    -- todo: cminus  
    Assign (CVar var) (CLess (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setl (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)
      
    Assign (CVar var) (CGreater (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setg (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)
      
    Assign (CVar var) (CEq (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Sete (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CEq (CVar a) (CVar b)) ->
      Cmpq (MemoryRef a) (MemoryRef b) : Sete (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks)

    Assign (CVar var) (CNot n) ->

      case n of
        CVar nvar ->
          Xorq  (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        CInt i ->
          Movq (Immediate i) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        CBool True ->
          Movq (Immediate 1) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks)
        _ -> Movq (Immediate 0) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks) 
      
    CLess (CVar var) (CInt n) ->
      Cmpq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
     --todo: cgreater
    Assign (CVar var) (CPlus (CVar var') (CVar var'')) ->
      Movq (MemoryRef var') (MemoryRef var) : Addq (MemoryRef var'') (MemoryRef var) : toselect (xs, blocks)
      
    Assign (CVar var) (CPlus (CVar var') (CInt n)) ->
      Movq (MemoryRef var') (MemoryRef var) : Addq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
      
    IfGoto (CVar var) (Goto block) (Goto block') ->
      let blk = Map.lookup block blocks
          blk' = Map.lookup block' blocks
      in case (blk, blk') of
          (Just blkInstrs, Just blk'Instrs) ->
            Cmpq (Immediate 1) (MemoryRef var) : Je block : Jmp block' : Label block : toselect (blkInstrs, blocks) ++  [Label block'] ++ toselect (blk'Instrs, blocks) ++ toselect (xs, blocks)
          _ -> error "Block not found"
          
    CInt n -> Movq (Immediate n) (Register "%rax") : toselect (xs, blocks)

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
             Label (block) : toselect (blk', blocks) ++ toselect ([cnd], blocks) ++ [Jmp block] ++ toselect (xs, blocks)
           _ -> error "Block not found"
    _ -> error "Unsupported CIR instruction"
