module Tox86 where

import Parser
import Desugar
import ToAnf
import ToCir
import ToSelect
import ToStack

import qualified Data.Map as Map

tox86 :: ([Instruction], Int) -> String
tox86 (((Label "garbage"):xs), stack) =
  let prelude = makeprelude stack in
    let tuppre = tox86' [Movq (Immediate 65536) (Register "%rdi"), Movq (Immediate 65536) (Register "%rsi"), Callq "initialize", Movq (MemoryRef "rootstack_begin(%rip)") (Register "%r15"), Movq (Immediate 0) (MemoryRef "0(%r15)"), Addq (Immediate 8) (Register "%r15")] in
      let conclusion = makeconclusion stack in
        let tupconclusion = "\tsubq $8, %r15\n" in
          prelude ++ tuppre ++ tox86' (patch ((Label "garbage"):xs) Map.empty) ++ tupconclusion ++ conclusion 
        
  
tox86 (ins, stack) =
  let prelude = makeprelude stack
      conclusion = makeconclusion stack
  in
    prelude ++ tox86' (patch ins Map.empty) ++ conclusion
    
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

tox86' ((Cmpq (Immediate e) (Register reg)):xs) =
  "\tcmpq " ++ "$" ++ show e ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Addq (Immediate e) (Register reg)):xs) =
  "\taddq " ++ "$" ++ show e ++ ", " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Movq (Register reg) (Register reg2)):xs) =
  "\tmovq " ++ reg ++ ", " ++ reg2 ++ "\n" ++ tox86' xs

tox86' ((Cmpq (Register reg) (Register reg2)):xs) =
  "\tcmpq " ++ reg ++ ", " ++ reg2 ++ "\n" ++ tox86' xs
  
tox86' ((Cmpq (Immediate e) (MemoryRef ref)):xs) =
  "\tcmpq " ++ "$" ++ show e ++ ", " ++ ref ++ "\n" ++ tox86' xs

tox86' ((Jl block):xs) =
  "\tjl " ++ block ++ "\n" ++ tox86' xs

tox86' ((Je block):xs) =
  "\tje " ++ block ++ "\n" ++ tox86' xs

tox86' ((Callq fn):xs) =
  "\tcallq " ++ fn ++ "\n" ++ tox86' xs

tox86' ((Addq (Immediate e) (MemoryRef ref)):xs) =
  "\taddq " ++ "$" ++ show e ++ ", " ++ ref ++ "\n" ++ tox86' xs

tox86' ((Setl (Register reg)):xs) =
  "\tsetl " ++ reg ++ "\n" ++ tox86' xs

tox86' ((Movzbq (Register reg) (Register reg2)):xs) =
  "\tmovzbq " ++ reg ++ ", " ++ reg2 ++ "\n" ++ tox86' xs

tox86' ((Jmp block):xs) =
  "\tjmp " ++ block ++ "\n" ++ tox86' xs


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

patch :: [Instruction] -> Map.Map String String -> [Instruction]
patch [] _ = []
  
patch ((Movzbq (Register reg) (MemoryRef ref)):xs) hash =
  let updatedHash = Map.insert ref "%rsi" hash in
    Movzbq (Register reg) (Register "%rsi") : patch xs updatedHash
    
patch ((Cmpq (Immediate e) (MemoryRef ref)):xs) hash =
  if Map.member ref hash 
    then Cmpq (Immediate e) (Register (hash Map.! ref)) : patch xs hash
    else Cmpq (Immediate e) (MemoryRef ref) : patch xs hash

patch (x:xs) hash = 
  x : patch xs hash
  
