module Tox86 where

import Parser
import Desugar
import ToAnf
import ToCir
import ToSelect
import ToStack

tox86 :: ([Instruction], Int) -> String
tox86 (ins, stack) =
  let prelude = makeprelude stack
      conclusion = makeconclusion stack
  in
    prelude ++ tox86' ins ++ conclusion
    
tox86' :: [Instruction] -> String
tox86' [] = ""

tox86' ((Movq (Immediate e) (MemoryRef ref)):xs) =
  "\tmovq " ++ "$" ++ show e ++ ", " ++ ref ++ "\n" ++ tox86' xs

tox86' ((Label block):xs) =
  block ++ ":" ++ "\n" ++ tox86' xs

tox86' ((Movq (MemoryRef ref) (Register reg)):xs) =
  "\tmovq " ++ ref ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Addq (MemoryRef ref) (Register reg)):xs) =
  "\taddq " ++ ref ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Addq (Register reg) (MemoryRef ref)):xs) =
  "\taddq " ++ reg ++ ", " ++ ref ++ "\n" ++ tox86' xs

tox86' ((Movq (Immediate e) (Register reg)):xs) =
  "\tmovq " ++ "$" ++ show e ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Addq (Immediate e) (Register reg)):xs) =
  "\taddq " ++ "$" ++ show e ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Movq (Register reg) (Register reg2)):xs) =
  "\tmovq " ++ reg ++ ", " ++ reg2 ++ "\n" ++ tox86' xs
  
tox86' ((Cmpq (Immediate e) (MemoryRef ref)):xs) =
  "\tcmpq " ++ "$" ++ show e ++ ", " ++ ref ++ "\n" ++ tox86' xs

tox86' ((Jl block):xs) =
  "\tjl " ++ block ++ "\n" ++ tox86' xs

tox86' ((Callq fn):xs) =
  "\tcallq " ++ fn ++ "\n" ++ tox86' xs

tox86' ((Addq (Immediate e) (MemoryRef ref)):xs) =
  "\taddq " ++ "$" ++ show e ++ ", " ++ ref ++ "\n" ++ tox86' xs

makeprelude :: Int -> String
makeprelude stack =
  let alignment = if (mod stack 16) == 0 then stack else stack + 8
  in
    "\t.globl main\n" ++ "main:\n" ++ "\tpushq %rbp\n" ++ "\tmovq %rsp, %rbp\n" ++ "\tsubq $" ++ show alignment ++ ", " ++ "%rsp\n"

makeconclusion :: Int -> String
makeconclusion stack =
  let alignment = if (mod stack 16) == 0 then stack else stack + 8
  in
    "\nconclusion:\n" ++ "\taddq $" ++ show alignment ++ ", " ++ "%rsp\n"  ++ "\tpopq %rbp\n" ++ "\tretq"
