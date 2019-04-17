# TODO

## Conflicts to Anticipate
1. Lambdas string * expr, not expr * expr 
2. Match against name mangled main in codegen rather than just "main"
3. **to implement** parse application of binary operators differently so we can have partial application
4. Remove lambda from main
5. Compiler.ml
6. remove_substs pass to turn iast into tast
7. ITE in codegen
8. Much of codegen.ml has been refactored into lib.ml
9. Application precedence moved in parser
10. tast and iast import miast

## Front-end
1. Create global variable and "fresh_ty_var" that generates fresh type variable for inference
2. Implement the rest of type inference in "infer_type" in passes/type_inference.ml
    - [use this as a source for type inference algorithm](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf)
    - add the equivalent of TypeEnv (type environment) as function argument
    - ~write equivalent mgu (most general union) function~
    - ~write equivalent apply function~
    - ~write equivalent ftv function (free type variable)~
    - ~create Subst struct/type~
3. Lift local named functions to the global scope
4. Perform lambda lifting (lift unnamed functions to toplevel lambda vdefs)
    - first capture unbound variables in lambda -> lambda args
    - [lambda lifting tutorial in Haskell](https://gist.github.com/jozefg/652f1d7407b7f0266ae9)
    - [high-level wiki algorithm on lifting](https://en.wikipedia.org/wiki/Lambda_lifting#Algorithm)

## Backend
1. Create struct to represent list comprehensions
2. Create struct to represent thunks
3. Rewrite codgen to generate loop to print lists
4. Write codegen to pattern match ITE and generate code
