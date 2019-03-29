open Ast

let rec infer_type _ = Tforall (["a"], Tvar "a")
