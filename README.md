# lispy

So far, the `ToAnf` module transforms `if` and `let` expressions, while loops, booleans, ints, and comparison operators such as `<,>, and, or, not, eq` to ANF.

The `ToCir` module lowers some nodes from an ANF AST to a graph like structure consisting of Gotos.

The `ToSelect` module takes a list of `Cir` and generates x86 instructions. 

# Examples

## Example 1

```haskell
ghci> ast 
If (If (If (Less (Int 2) (Int 3)) (Less (Int 2) (Int 100)) (Less (Int 1) (Int 1000))) (Less (Int 2) (Int 3)) (Less (Int 2) (Int 3))) (Int 1) (Int 2)
ghci> anf
MLet [(AVar "temp_0",MLet [(AVar "temp_2",MLet [(AVar "temp_4",MLess (AInt 2) (AInt 3))] (MIf (AExp (AVar "temp_4")) (MLess (AInt 2) (AInt 100)) (MLess (AInt 1) (AInt 1000))))] (MIf (AExp (AVar "temp_2")) (MLess (AInt 2) (AInt 3)) (MLess (AInt 2) (AInt 3))))] (MIf (AExp (AVar "temp_0")) (AExp (AInt 1)) (AExp (AInt 2)))
ghci> e
([Assign (CVar "temp_4") (CLess (CInt 2) (CInt 3)),IfGoto (CVar "temp_4") (Goto "block_0") (Goto "block_1"),IfGoto (CVar "temp_2") (Goto "block_2") (Goto "block_3"),IfGoto (CVar "temp_0") (Goto "block_4") (Goto "block_5"),IfGoto (CVar "temp_0") (Goto "block_6") (Goto "block_7")],fromList [("block_0",[Assign (CVar "temp_2") (CLess (CInt 2) (CInt 100))]),("block_1",[Assign (CVar "temp_2") (CLess (CInt 1) (CInt 1000))]),("block_2",[Assign (CVar "temp_0") (CLess (CInt 2) (CInt 3))]),("block_3",[Assign (CVar "temp_0") (CLess (CInt 2) (CInt 3))]),("block_4",[CInt 1]),("block_5",[CInt 2]),("block_6",[CInt 1]),("block_7",[CInt 2])])
ghci> toselect e
[Cmpq (Immediate 2) (Immediate 3),Setl (Register "%al"),Movzbq (Register "%al") (MemoryRef "temp_4"),Cmpq (Immediate 1) (MemoryRef "temp_4"),Je "block_0",Jmp "block_1",Label "block_0",Cmpq (Immediate 2) (Immediate 100),Setl (Register "%al"),Movzbq (Register "%al") (MemoryRef "temp_2"),Label "block_1",Cmpq (Immediate 1) (Immediate 1000),Setl (Register "%al"),Movzbq (Register "%al") (MemoryRef "temp_2"),Cmpq (Immediate 1) (MemoryRef "temp_2"),Je "block_2",Jmp "block_3",Label "block_2",Cmpq (Immediate 2) (Immediate 3),Setl (Register "%al"),Movzbq (Register "%al") (MemoryRef "temp_0"),Label "block_3",Cmpq (Immediate 2) (Immediate 3),Setl (Register "%al"),Movzbq (Register "%al") (MemoryRef "temp_0"),Cmpq (Immediate 1) (MemoryRef "temp_0"),Je "block_4",Jmp "block_5",Label "block_4",Movq (Immediate 1) (Register "%rax"),Label "block_5",Movq (Immediate 2) (Register "%rax"),Cmpq (Immediate 1) (MemoryRef "temp_0"),Je "block_6",Jmp "block_7",Label "block_6",Movq (Immediate 1) (Register "%rax"),Label "block_7",Movq (Immediate 2) (Register "%rax")]
```

## Example 2
```haskell
let ast = parseExp (lexer "(let ((sum 0)) (let ((i 0)) (begin (while (< i 5) (begin (set sum (+ sum i)) (set i (+ i 1)))) sum)))")
ghci> ast
Let [(Var "sum",Int 0)] (Let [(Var "i",Int 0)] (Begin [While (Less (Var "i") (Int 5)) (Begin [Set (Var "sum") (Plus (Var "sum") (Var "i")),Set (Var "i") (Plus (Var "i") (Int 1))]),Var "sum"]))
ghci> anf = toanf ast 
ghci> anf
MLet [(AVar "sum",AExp (AInt 0))] (MLet [(AVar "i",AExp (AInt 0))] (MBegin [MWhileLoop (MLess (AVar "i") (AInt 5)) (MBegin [MSetBang (AVar "sum") (MPlus (AVar "sum") (AVar "i")),MSetBang (AVar "i") (MPlus (AVar "i") (AInt 1))]),AExp (AVar "sum")]))
ghci> toc = makeexplicit anf
ghci> toc
([Assign (CVar "sum") (CInt 0),Assign (CVar "i") (CInt 0),IfGotoLoop (CLess (CVar "i") (CInt 5)) (Goto "loop_0")],fromList [("loop_0",[CBegin [Assign (CVar "sum") (CPlus (CVar "sum") (CVar "i")),Assign (CVar "i") (CPlus (CVar "i") (CInt 1))]])])
ghci> toselect toc
[Movq (Immediate 0) (MemoryRef "sum"),Movq (Immediate 0) (MemoryRef "i"),Label "loop_0",Movq (MemoryRef "sum") (MemoryRef "sum"),Addq (MemoryRef "i") (MemoryRef "sum"),Movq (MemoryRef "i") (MemoryRef "i"),Addq (Immediate 1) (MemoryRef "i"),Cmpq (Immediate 5) (MemoryRef "i"),Jmp "loop_0"]
```
# thanks
