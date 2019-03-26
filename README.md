# rippl

NOTES: Current parsing only supports desugared syntax (no proper function definitions; only lambdas)

Compiler passes:
  * Semantic checks
    - check that clauses in list comprehensions make sense
    - type checking
    - check that there are no type annotations for non-declared vdefs
    - check that there is a "main" vdef present
  * AST transformations
    - closures of lambdas (no unbound variables)
    - lambda lifting (no more anonymous functions)
    - type inference
