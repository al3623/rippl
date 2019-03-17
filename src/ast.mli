type ty = Int | Bool | Float | Char
    | Tvar of string
    | Arrow of (ty * ty)
    | TconList of ty
    | TconTuple of ty
    | Tforall of ty

type expr =
    | IntLit of int | FloatLit of float | BoolLit of bool 
    | CharLit of char | ListLit of expr list
    | Add | Sub | Mult | Div | Mod | Pow
    | AddF | SubF | MultF | DivF | PowF
    | Eq | EqF | Neq | NeqF | Geq | GeqF | Leq | LeqF
    | Less | LessF | Greater | GreaterF
    | And | Or | Not 
    | Cons | Cat | Len | Head | Tail
    | Var of string
    | Let of (expr * expr)
    | Lambda of (expr * expr)
    | App of (expr * expr)
    | Ite of (expr * expr * expr)
    | Assign of (expr * expr)

type clause = 
	| ListVBind of (expr * expr)
	| Filter of expr

type listcomp = ListComp of (expr * clause list)

type annot  = 
    | Annot of (string * ty) 
