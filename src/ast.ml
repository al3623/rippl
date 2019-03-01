type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq 
    | And | Or | Mod | Cons | Cat | Len | Head | Tail

type ty = Int | Bool | Float | Char
    | Tvar of string
    | Arrow of (ty * ty)
    | TconList of ty
    | TconTuple of ty
    | Tforall of ty
