open Ast
open Miast

(*
type typed_expr = (typed_expr * ty)

type typed_decl = TypedVdef of (string * typed_expr)

type typed_program = typed_decl list
*)
type inferred_expr = Ast.ty SubstMap.t * ix * ty
and
ix =
    | IIntLit of int | IFloatLit of float | IBoolLit of bool 
    | ICharLit of char | IWildCard
    | IAdd | ISub | IMult | IDiv | IMod | IPow
    | IAddF | ISubF | IMultF | IDivF | IPowF | INeg
    | IEq | IEqF | INeq | INeqF | IGeq | IGeqF | ILeq | ILeqF
    | ILess | ILessF | IGreater | IGreaterF
    | IAnd | IOr | INot 
    | ICons | ICat | ILen | IHead | ITail 
	| IFirst | ISec
	| ITuple of (inferred_expr * inferred_expr)
	| IIs_none | IFrom_just | IJust of inferred_expr | None
    | IVar of (string)
    | ILet of (Ast.ty SubstMap.t * iassign * inferred_expr)
    | ILambda of (Ast.ty SubstMap.t * string * inferred_expr)
    | IApp of (Ast.ty SubstMap.t * inferred_expr * inferred_expr)
    | IIte of (Ast.ty SubstMap.t * inferred_expr * inferred_expr * inferred_expr)
	| IListComp of (Ast.ty SubstMap.t * inferred_expr * iclause list)
	| IListRange of (Ast.ty SubstMap.t * inferred_expr * inferred_expr)
	| IInfList of (Ast.ty SubstMap.t * inferred_expr)
    | IListLit of (inferred_expr list)
and iclause = 
	| IListVBind of (string * inferred_expr)
	| IFilter of inferred_expr
and iassign = IAssign of (string * inferred_expr)

type inferred_decl =
    | InferredVdef of (string * inferred_expr)
