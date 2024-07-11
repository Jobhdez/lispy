# rackety

Will be a compiler for a subset of a Racket like compiler.

so far, the `ToAnf` module transforms `if` and `let` expressions, while loops, booleans, ints, and comparison operators such as `<,>, and, or, not, eq` to ANF.

the `ToCir` lowers some node from an ANF ast to a graph like structure consisting of Gotos.

# thanks