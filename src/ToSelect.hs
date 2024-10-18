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

toselect :: ([Cir], Map.Map String [Cir]) -> Int -> [Instruction]
toselect ([], _) _ = []
toselect ((x:xs), blocks) counter =
  case x of
    Assign (CVar var) (CInt n) ->
      Movq (Immediate n) (MemoryRef var) : toselect (xs, blocks) counter
      
    CPlus (CVar var) (CVar var') ->
      Movq (MemoryRef var) (Register "%rax") : Addq (Register "%rax") (MemoryRef var') : Movq (MemoryRef var') (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CPlus (CVar var) (CInt e) ->
      Addq (Immediate e) (MemoryRef var) : Movq (MemoryRef var) (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CPlus (CInt e) (CVar var) ->
      Addq (Immediate e) (MemoryRef var) : Movq (MemoryRef var) (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CPlus (CInt e) (CInt e2) ->
      Movq (Immediate e) (Register "%rax") : Addq (Immediate e2) (Register "%rax") : Movq (Register "%rax") (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CMinus (CVar var) (CVar var') ->
      Movq (MemoryRef var) (Register "%rax") : Subq (Register "%rax") (MemoryRef var') : toselect (xs, blocks) counter

    CMinus (CVar var) (CInt e) ->
      [Subq (Immediate e) (MemoryRef var)]

    CMinus (CInt e) (CVar var) ->
      [Subq (Immediate e) (MemoryRef var)]

    CMinus (CInt e) (CInt e2) ->
      Movq (Immediate e) (Register "%rax") : Subq (Immediate e2) (Register "%rax") : toselect (xs, blocks) counter

    CLess (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) : toselect (xs, blocks) counter

    CLess (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var) : toselect (xs, blocks) counter

    CLess (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e)  : toselect (xs, blocks) counter

    CLess (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e)  : toselect (xs, blocks) counter
---
    CGreater (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) :  toselect (xs, blocks) counter

    CGreater (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var) : toselect (xs, blocks) counter

    CGreater (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e) : toselect (xs, blocks) counter

    CGreater (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e)  : toselect (xs, blocks) counter
---
    CEq (CVar var) (CVar var') ->
      Movq (MemoryRef var') (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef var)) :  toselect (xs, blocks) counter

    CEq (CVar var) (CInt e) ->
      Cmpq (Immediate e) (MemoryRef var)  : toselect (xs, blocks) counter

    CEq (CInt e) (CVar var) ->
      Cmpq (MemoryRef var) (Immediate e) : toselect (xs, blocks) counter

    CEq (CInt e) (CInt e2) ->
      Cmpq (Immediate e2) (Immediate e) : toselect (xs, blocks) counter
    
    CEq (CVar var) (CBool e) ->
      let bool = if e == True then 1 else 0 in
        Cmpq (Immediate bool) (MemoryRef var) : toselect (xs, blocks) counter

    CEq (CBool e) (CVar var) ->
      let bool = if e == True then 1 else 0 in
        Cmpq (MemoryRef var) (Immediate bool) : toselect (xs, blocks) counter

    CEq (CBool e) (CBool b) ->
      let bool1 = if e == True then 1 else 0 in
        let bool2 = if b == True then 1 else 0 in
          Cmpq (Immediate bool2) (Immediate bool1) : toselect (xs, blocks) counter
      
    -- todo: cminus  
    Assign (CVar var) (CLess (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setl (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CLess (CInt a) (CVar b)) ->
       Cmpq (MemoryRef b) (Immediate a) : Setl (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CLess (CVar b) (CInt a)) ->
      Cmpq (Immediate a) (MemoryRef b) : Setl (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CLess (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Setl (Register "%al")): (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks) counter
      
  ---
    Assign (CVar var) (CGreater (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Setg (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CGreater (CInt a) (CVar b)) ->
       Cmpq (MemoryRef b) (Immediate a) : Setg (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CGreater (CVar a) (CInt b)) ->
       Cmpq (Immediate b) (MemoryRef a) : Setg (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CGreater (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Setg (Register "%al")): (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks) counter

    Assign (CVar var) (CPlus (CInt a) (CInt b)) ->
       Movq (Immediate a) (MemoryRef var) : Addq (Immediate b) (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CPlus (CInt a) (CVar b)) ->
      Movq (Immediate a) (Register "%rax") : Addq (Register "%rax") (MemoryRef b) : toselect (xs, blocks) counter

    Assign (CVar var) (CPlus (CVar b) (CInt a)) ->
      Movq (Immediate a) (Register "%rax") : Addq (Register "%rax") (MemoryRef b) : toselect (xs, blocks) counter

    Assign (CVar var) (CPlus (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : Addq (Register "%rax") (MemoryRef a): toselect (xs, blocks) counter

    --

    Assign (CVar var) (CMinus (CInt a) (CInt b)) ->
       Movq (Immediate a) (MemoryRef var) : Subq (Immediate b) (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CMinus (CInt a) (CVar b)) ->
      Movq (Immediate a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks) counter

    Assign (CVar var) (CMinus (CVar b) (CInt a)) ->
      Movq (Immediate a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks) counter

    Assign (CVar var) (CMinus (CVar a) (CVar b)) ->
      Movq (MemoryRef a) (Register "%rax") : Subq (MemoryRef b) (Register "%rax") : toselect (xs, blocks) counter
    
      
    Assign (CVar var) (CEq (CInt a) (CInt b)) ->
      Cmpq (Immediate a) (Immediate b) : Sete (Register "%al") : Movzbq (Register "%al") (MemoryRef var) : toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CVar a) (CVar b)) ->
      Movq (MemoryRef b) (Register "%rax") : (Cmpq (Register "%rax") (MemoryRef a)) : (Sete (Register "%al")) : (Movzbq (Register "%al") (MemoryRef var)) :  toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CVar a) (CInt b)) ->
      Cmpq (Immediate b) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CInt b) (CVar a)) ->
      Cmpq (Immediate b) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CBool b) (CVar a)) ->
      let bool = if b == True then 1 else 0 in
        Cmpq (MemoryRef a) (Immediate bool) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CVar a) (CBool b)) ->
      let bool = if b == True then 1 else 0 in
        Cmpq (Immediate bool) (MemoryRef a) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter

    Assign (CVar var) (CEq (CBool a) (CBool b)) ->
      let bool1 = if a == True then 1 else 0 in
        let bool2 = if b == True then 1 else 0 in
          Cmpq (Immediate bool2) (Immediate bool1) : Sete (Register "%al") : (Movzbq (Register "%al") (MemoryRef var)) : toselect (xs, blocks) counter
                                                                     
    Assign (CVar var) (CNot n) ->

      case n of
        CVar nvar ->
          Xorq  (Immediate 1) (MemoryRef var) : toselect (xs, blocks) counter
        CInt i ->
          Movq (Immediate i) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks) counter
        CBool True ->
          Movq (Immediate 1) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks) counter
        _ -> Movq (Immediate 0) (MemoryRef var) : Xorq (Immediate 1) (MemoryRef var) : toselect (xs, blocks) counter
        
    Assign (CVar t) (CTup entries) ->
      toselect ([(CTup entries)], blocks) counter ++  toselect (xs, blocks) counter 
    
    IfGoto (CVar var) (Goto block) (Goto block') ->
      let blk = Map.lookup block blocks
          blk' = Map.lookup block' blocks
      in case (blk, blk') of
          (Just blkInstrs, Just blk'Instrs) ->
            Cmpq (Immediate 1) (MemoryRef var) : Je block : Jmp block' : Label block : toselect (blkInstrs, blocks) counter ++ [Jmp "conclusion"] ++  [Label block'] ++ toselect (blk'Instrs, blocks) counter ++ [Jmp "conclusion"] ++ toselect (xs, blocks) counter
          _ -> error "Block not found"
          
    CInt n -> Movq (Immediate n) (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CVar var -> Movq (MemoryRef var) (Register "%rdi") : Callq "print_int" : toselect (xs, blocks) counter

    CBool b ->
      if b == True
      then Movq (Immediate 1) (Register "%rax") : toselect (xs, blocks) counter
      else
        Movq (Immediate 0) (Register "%rax") : toselect (xs, blocks) counter
    
    CBegin exps -> concatMap (\cirexp -> toselect ([cirexp], blocks) counter) exps ++ toselect (xs, blocks) counter

    (IfStmt (CLesst (CAddt (CGlobalValue ptr) bytes) (CGlobalValue from)) (CVoid v) (CCollect bytes2)) ->
      Label "garbage" : Movq (MemoryRef "free_ptr(%rip)") (Register "%rax") : Addq (Immediate bytes) (Register "%rax") : Movq (MemoryRef "fromspace_end(%rip)") (Register "%r13") : Cmpq (Register "%r13") (Register "%rax") : Jl "garbage_block_1" : [Jmp "garbage_block_2"] ++ [Label "garbage_block_1"] ++ [Movq (Immediate 0) (Register "%r13")] ++ [Jmp "garbage_block_3"] ++ [Label "garbage_block_2"] ++ [Movq (Register "%r15") (Register "%rdi")] ++ [Movq (Immediate bytes) (Register "%rsi")] ++ [Callq "collect"] ++ [Jmp "garbage_block_3"] ++ [Label "garbage_block_3"] ++ toselect (xs, blocks) counter
      
    (CDefine tup (CAllocate n)) ->
      let tuplelength = 8 * (n + 1) in
        let tag = makeTag n in
          [Movq (MemoryRef "free_ptr(%rip)") (Register "%r11")] ++ [Addq (Immediate tuplelength) (MemoryRef "free_ptr(%rip)")] ++ [Movq (Immediate tag) (Register "0(%r11)")] ++ toselect (xs, blocks) counter

    (CVecSet tup index (CInt n)) ->
      let newcounter = counter + 8 in
        [Movq (Immediate n) (Register ((show newcounter) ++ "(%r11)"))] ++ toselect (xs, blocks) newcounter

    (CTupRef (CVar tup) index) ->
      let nindex = 8 * (index + 1) in
        [Movq (Register ((show nindex) ++ "(%r11)")) (Register "%rdi")] ++ [Callq "print_int"] ++ toselect (xs, blocks) counter
        
    (CTup (y:ys)) ->
      toselect ([y], blocks) counter ++ rest
      where
        rest = toselect (ys,blocks) counter 
      

    
    IfGotoLoop cnd (Goto block) ->
      let blk = Map.lookup block blocks
      in case blk of
           (Just blk') ->
             Label (block) : toselect (blk', blocks) counter ++ toselect ([cnd], blocks) counter ++ [Jl block] ++ toselect (xs, blocks) counter
           _ -> error "Block not found"
    _ -> error "Unsupported CIR instruction"



    
makeTag :: Int -> Int
makeTag lengthTup =
  let makePointerMask :: Int  -> Int -> String -> String
      makePointerMask 0 counter str = reverse str
      makePointerMask length counter str =
       {- the point mask corresponds to a sequence of bits; if
          the entry in the tuple is an int then return "0"; if its
           another tuple return "1".-}
        let str' = str ++ (show counter) in
          str' ++ makePointerMask (length - 1) counter str'
      
      tupleLengthToBits :: Int -> String
      tupleLengthToBits len = let binaryStr = reverse (go len)
                                  go 0 = "0"
                                  go n = if n `mod` 2 == 1 then '1' : go (n `div` 2) else '0' : go (n `div` 2)
                              in replicate (6 - length binaryStr) '0' ++ binaryStr

      tagBitsToDecimal :: String -> Int
      tagBitsToDecimal bits = go (reverse bits) 0
        where
          go [] _ = 0
          go (x:xs) n = (if x == '1' then 2 ^ n else 0) + go xs (n+1)

  in let len = tupleLengthToBits lengthTup
         pointerMask = makePointerMask lengthTup 0  ""
         fwdPtr = "1"
     in tagBitsToDecimal (pointerMask ++ len ++ fwdPtr)
      
