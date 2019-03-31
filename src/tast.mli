open Ast
(*
type typed_expr = (typed_expr * ty)

type typed_decl = TypedVdef of (string * typed_expr)

type typed_program = typed_decl list
*)
type typed_expr =
    | TIntLit of int | TFloatLit of float | TBoolLit of bool 
    | TCharLit of char
    | TAdd | TSub | TMult | TDiv | TMod | TPow
    | TAddF | TSubF | TMultF | TDivF | TPowF | TNeg
    | TEq | TEqF | TNeq | TNeqF | TGeq | TGeqF | TLeq | TLeqF
    | TLess | TLessF | TGreater | TGreaterF
    | TAnd | TOr | TNot 
    | TCons | TCat | TLen | THead | TTail
    | TVar of string
    | TLet of (tassign * typed_expr * ty)
    | TLambda of (typed_expr * typed_expr * ty)
    | TApp of (typed_expr * typed_expr * ty)
    | TIte of (typed_expr * typed_expr * typed_expr * ty)
	| TListComp of (typed_expr * tclause list * ty)
	| TListRange of (typed_expr * typed_expr * ty)
	| TInfList of (typed_expr * ty)
    | TListLit of (typed_expr list * ty)
and tclause = 
	| TListVBind of (typed_expr * typed_expr)
	| TFilter of typed_expr
and tassign = TAssign of (typed_expr * typed_expr * ty)

type typed_decl =
    | TypedVdef of (string * typed_expr)
