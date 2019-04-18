open Ast
open Tast
open Pretty_type_print

    
let rec texpr_to_str tx =
    match tx with
    | TIntLit (i) -> string_of_int i
    | TFloatLit (f) -> string_of_float f
    | TBoolLit (b) -> string_of_bool b
    | TCharLit (c) -> String.make 1 c
    | TWildCard -> "_"
    | _ -> "texpr"

let typ_to_str ty =
    Pretty_type_print.ty_to_str ty

let rec tast_to_str tdec = 
    match tdec with
    | (TypedVdef (name, (texpr1, infty))) -> "TypedVdef(" ^ (name) ^ ", (" ^ (texpr_to_str texpr1) ^ ", " ^ (typ_to_str infty) ^ "))"


