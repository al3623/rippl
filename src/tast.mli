open Ast

type program = decl list

type typed_expr = TypedExpr of (expr * ty)

type typed_decl = TypedVdef of (string * typed_expr)

type typed_program = typed_decl list
