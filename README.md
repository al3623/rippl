# rippl

NOTES: Current parsing only supports desugared syntax (no proper function definitions; only lambdas)

Parsing Updates: @Da
 - make test script read in entire file to test (not line by line)
 - make epilogue process multiple vdefs/annotations in a row
 - make more parsing test cases

Compiler passes:
  * Semantic checks
    - check that clauses in list comprehensions make sense @Hollis
    - type checking @Amanda
    - check that there are no type annotations for non-declared vdefs @Hans
    - ~check that there is a "main" vdef present~
  * AST transformations
    - closures of lambdas (no unbound variables)
    - lambda lifting (no more anonymous functions)
    - lift local function definitions to global definitions
    - type inference
