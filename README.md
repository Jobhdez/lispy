# rackety

So far, the `ToAnf` module transforms `if` and `let` expressions, while loops, booleans, ints, and comparison operators such as `<,>, and, or, not, eq` to ANF.

The `ToCir` module lowers some nodes from an ANF AST to a graph like structure consisting of Gotos.

The `ToSelect` module takes a list of `Cir` and generates x86 instructions. 

# thanks
