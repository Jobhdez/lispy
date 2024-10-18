module ToStack where

import Parser
import Desugar
import ToAnf
import ToCir
import ToSelect
import qualified Data.Map as Map

toStack :: [Instruction] -> ([Instruction], Int)
toStack ins =
  toStack' ins 0 Map.empty

toStack' :: [Instruction] -> Int -> Map.Map String String -> ([Instruction], Int)
toStack' [] counter _ = ([], counter)

toStack' ((Movq (MemoryRef "free_ptr(%rip)") reg):xs) counter hashmap =
  let (rest, finalCounter) = toStack' xs counter hashmap in
    (Movq (MemoryRef "free_ptr(%rip)") reg : rest, finalCounter)

toStack' ((Movq (MemoryRef "fromspace_end(%rip)") reg):xs) counter hashmap =
  let (rest, finalCounter) =  toStack' xs counter hashmap in
    (Movq (MemoryRef "fromspace_end(%rip)") reg : rest, finalCounter)

toStack' ((Addq imm (MemoryRef "free_ptr(%rip)")):xs) counter hashmap =
  let (rest, finalCounter) =  toStack' xs counter hashmap in
    (Addq imm (MemoryRef "free_ptr(%rip)") : rest, finalCounter)
  
toStack' ((Movq (Immediate e) (MemoryRef ref)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Movq (Immediate e) (MemoryRef stacklocation) : rest, finalCounter)

toStack' ((Movq (MemoryRef ref) (Register reg)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Movq (MemoryRef stacklocation) (Register reg) : rest, finalCounter)

toStack' ((Addq (MemoryRef ref) (Register reg)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Addq (MemoryRef stacklocation) (Register reg) : rest, finalCounter)

toStack' ((Cmpq (Immediate e) (MemoryRef ref)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Cmpq (Immediate e) (MemoryRef stacklocation) : rest, finalCounter)

toStack' ((Addq (Register reg) (MemoryRef ref)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Addq (Register reg) (MemoryRef stacklocation) : rest, finalCounter)

toStack' ((Addq (Immediate e) (MemoryRef ref)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Addq (Immediate e) (MemoryRef stacklocation) : rest, finalCounter)

toStack' ((Movzbq (Register reg) (MemoryRef ref)):xs) counter hashmap =
  let (stacklocation, counter', hashmap') =
        if Map.member ref hashmap
        then (hashmap Map.! ref, counter, hashmap)
        else let counter' = counter + 8
                 stacklocation' = "-" ++ show counter' ++ "(%rbp)"
                 hashmap' = Map.insert ref stacklocation' hashmap
             in  (stacklocation', counter', hashmap')
      (rest, finalCounter) = toStack' xs counter' hashmap'
  in
    (Movzbq (Register reg) (MemoryRef stacklocation) : rest, finalCounter)

    
toStack' (x:xs) counter hashmap =
  let (rest, finalCounter) = toStack' xs counter hashmap
  in (x : rest, finalCounter)
