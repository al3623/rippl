# rippl

NOTES: Current parsing only supports desugared syntax (no proper function definitions; only lambdas)

Questions for Ryan:
 - associativity of the application operator

Parsing Updates:
 - make test script read in entire file to test (not line by line)
 - make epilogue process multiple vdefs/annotations in a row

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
