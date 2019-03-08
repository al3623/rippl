type ty = Int | Bool | Float | Char
    | Tvar of string
    | Arrow of (ty * ty)
    | TconList of ty
    | TconTuple of ty
    | Tforall of ty

type expr =
    | IntLit of Int | FloatLit of Float | TLit of Bool 
    | FLit of Bool | CharLit of Char | StrLit of string
    | Add | Sub | Mult | Div | Mod | Pow
    | AddF | SubF | MultF | DivF | PowF
    | Eq | EqF | Neq | NeqF | Geq | GeqF | Leq | LeqF
    | And | Or 
    | Cons | Cat | Len | Head | Tail
    | Var of string
    | Let of (expr * expr)
    | Lambda of (expr * expr)
    | App of (expr * expr)
    | Ite of (expr * expr * expr) 
