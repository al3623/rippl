type expr =
    | IntLit of int | FloatLit of float | BoolLit of bool 
    | CharLit of char | WildCard
    | Add | Sub | Mult | Div | Mod | Pow
    | AddF | SubF | MultF | DivF | PowF | Neg
    | Eq | EqF | Neq | NeqF | Geq | GeqF | Leq | LeqF
    | Less | LessF | Greater | GreaterF
    | And | Or | Not 
    | Cons | Cat | Len | Head | Tail 
	(* Tuple operations *)
	| First | Sec
	| Tuple of (expr * expr)
	(* Maybe operations *)
	| Is_none | From_just | Just of expr | None
    | Var of string
    | Let of (assign * expr)
    | Lambda of (string * expr)
    | App of (expr * expr)
    | Ite of (expr * expr * expr)
	| ListComp of (expr * clause list)
	| ListRange of (expr * expr)
	| InfList of expr
    | ListLit of expr list
and clause = 
	| ListVBind of (string * expr)
	| Filter of expr
and assign = Assign of (string * expr)

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

type program = decl list

type lambda_def = {
    lname: string;
    ltyp: ty;
    rtyp: ty;
    lexp: expr;
    rexp: expr;
}

