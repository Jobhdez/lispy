# lispy

can compile some basic scheme like expressions :-) such as loops, if expressions, tuples.

### Programs that can be compiled ...

Navigate to `src/Compile.hs` and you will see some  main functions commented out. Comment all of them except for the program you want to compile and then:
```
$ ghci Compile.hs
> Compile.main
```
and then you execute the assembly by
```
$ clang -c -g runtime/runtime.c
$ clang -g runtime.o assemblyfile.s
```

```haskell
main = do
  let asm = compile "(let ((i 0)) (if (< i 3) 3 4))" in
    writeToFile "ifeg.s" asm

main = do
  let asm = compile "(let ((x 3)) (let ((y 4)) (+ x y)))" in
    writeToFile "egsumvar.s" asm
   
    
main = do
  let asm = compile "(+ 3 4)" in
    writeToFile "egsum.s" asm

main = do
  let asm = compile "(let ((i 3)) (+ i 4))" in
    writeToFile "eg.s" asm
  

main = do
  let asm = compile "(let ((sum 0)) (let ((i 0)) (begin (while (< i 5) (begin (set sum (+ sum i)) (set i (+ i 1)))) sum)))" in
    writeToFile "example2.s" asm

main = do
  let asm = compile "(let ((tup (vector 1 2 3))) (vectorref tup 2))" in
    writeToFile "tup.s" asm
	
```
