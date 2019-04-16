open Ast

(*
type typed_expr = (typed_expr * ty)

type typed_decl = TypedVdef of (string * typed_expr)

type typed_program = typed_decl list
*)
type typed_expr = tx * ty
and
tx =
    | TIntLit of int | TFloatLit of float | TBoolLit of bool 
    | TCharLit of char | TWildCard
    | TAdd | TSub | TMult | TDiv | TMod | TPow
    | TAddF | TSubF | TMultF | TDivF | TPowF | TNeg
    | TEq | TEqF | TNeq | TNeqF | TGeq | TGeqF | TLeq | TLeqF
    | TLess | TLessF | TGreater | TGreaterF
    | TAnd | TOr | TNot 
    | TCons | TCat | TLen | THead | TTail
    | TVar of string
    | TLet of (tassign * typed_expr)
    | TLambda of (typed_expr * typed_expr)
    | TApp of (typed_expr * typed_expr)
    | TIte of (typed_expr * typed_expr * typed_expr)
	| TListComp of (typed_expr * tclause list)
	| TListRange of (typed_expr * typed_expr)
	| TInfList of typed_expr
    | TListLit of typed_expr list
and tclause = 
	| TListVBind of (typed_expr * typed_expr)
	| TFilter of typed_expr
and tassign = TAssign of (typed_expr * typed_expr)

type typed_decl =
    | TypedVdef of (string * typed_expr)
