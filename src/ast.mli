type expr =
    | IntLit of int | FloatLit of float | BoolLit of bool 
    | CharLit of char | WildCard
    | Add | Sub | Mult | Div | Mod | Pow
    | AddF | SubF | MultF | DivF | PowF | Neg
    | Eq | EqF | Neq | NeqF | Geq | GeqF | Leq | LeqF
    | Less | LessF | Greater | GreaterF
    | And | Or | Not 
    | Cons | Cat | Len | Head | Tai
    | Var of string
    | Let of (expr * expr)
    | Lambda of (expr * expr)
    | App of (expr * expr)
    | Ite of (expr * expr * expr)
    | Assign of (expr * expr)
	| ListComp of (expr * clause list)
	| ListRange of (expr * expr)
	| InfList of expr
    | ListLit of expr list
and clause = 
	| ListVBind of (expr * expr)
	| Filter of expr

type decl =
    | Annot of (string * ty)
    | Vdef of (string * expr)
and ty = Int | Bool | Float | Char
    | Tvar of string
    | Tarrow of (ty * ty)
    | TconList of ty
    | TconTuple of (ty * ty)
    | Tforall of ((string list) * ty)
    | Tmaybe of ty


