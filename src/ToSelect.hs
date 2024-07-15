module ToSelect where

import Parser
import ToAnf
import ToCir
import qualified Data.Map as Map

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
  | Callq String Int
  | Retq
  | Cmpq Argument Argument
  | Incq Argument
  | Setg Argument
  | Movzbl Argument Argument
  | Label String
  deriving Show

toselect :: ([Cir], Map.Map String [Cir]) -> [Instruction]
toselect ([], _) = []
toselect ((x:xs), blocks) =
  case x of
    Assign (CVar var) (CInt n) ->
      Movq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
      
    CPlus (CVar var) (CVar var') ->
      Addq (MemoryRef var) (MemoryRef var') : toselect (xs, blocks)
      
    Assign (CVar var) (CLess (CInt a) (CInt b)) ->
      Cmpq (Immediate b) (Immediate a) : Setg (Register "%al") : Movzbl (Register "%al") (MemoryRef var) : toselect (xs, blocks)
      
    CLess (CVar var) (CInt n) ->
      Cmpq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
      
    Assign (CVar var) (CPlus (CVar var') (CVar var'')) ->
      Movq (MemoryRef var') (MemoryRef var) : Addq (MemoryRef var'') (MemoryRef var) : toselect (xs, blocks)
      
    Assign (CVar var) (CPlus (CVar var') (CInt n)) ->
      Movq (MemoryRef var') (MemoryRef var) : Addq (Immediate n) (MemoryRef var) : toselect (xs, blocks)
      
    IfGoto (CVar var) (Goto block) (Goto block') ->
      let blk = Map.lookup block blocks
          blk' = Map.lookup block' blocks
      in case (blk, blk') of
          (Just blkInstrs, Just blk'Instrs) ->
            Cmpq (Immediate 1) (MemoryRef var) : Jmp block : Label block : toselect (blkInstrs, blocks) ++ [Jmp block'] ++ Label block' : toselect (blk'Instrs, blocks) ++ toselect (xs, blocks)
          _ -> error "Block not found"
          
    CInt n -> Movq (Immediate n) (Register "%rax") : toselect (xs, blocks)
    
    CBegin exps -> concatMap (\cirexp -> toselect ([cirexp], blocks)) exps ++ toselect (xs, blocks)
    
    IfGotoLoop cnd (Goto block) ->
      let blk = Map.lookup block blocks
      in case blk of
           (Just blk') ->
             Label (block) : toselect (blk', blocks) ++ toselect ([cnd], blocks) ++ [Jmp block] ++ toselect (xs, blocks)
           _ -> error "Block not found"
    _ -> error "Unsupported CIR instruction"
