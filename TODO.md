# TODO

## To Go Over
1. Codegen for lifted vdefs and how to use struct Thunks
2. Changed closure for lifting to take care of lifting

## Front-end
1. Parse tuple operators (first, sec)
2. Parse maybe operators (just, is_nothing, from_just)
3. Parse list operators (map, filter)
4. Write type inference for list comprehensions
5. Add (first, sec, just, is_nothing, from_just, map, filter) types to inference
6. IMPORTANT: parsing does weird stuff with application in Ite (see test6.rpl in tests/inference/inference_tests)

## Backend
1. ~Create struct to represent list comprehensions~
2. ~Create struct to represent thunks~
3. ~Rewrite codgen to generate loop to print lists~
4. ~Write codegen to pattern match ITE and generate code~
5. Implement cons, head, tail, cat, len, in lib.c and lib.ml
6. Implement first, sec, is_nothing, from_just in lib.c and lib.ml
