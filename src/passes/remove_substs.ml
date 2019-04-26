open Iast
open Tast

let rec remove_subst_expr = function
	| (_,IIntLit i,t) -> (TIntLit i,t)
	| (_,IFloatLit f,t) -> (TFloatLit f, t)
	| (_,IBoolLit b,t) -> (TBoolLit b,t)
    | (_,ICharLit c,t) -> (TCharLit c,t)
	| (_,ITuple(ix1,ix2),t) -> 
		(TTuple(remove_subst_expr ix1, remove_subst_expr ix2), t)
	| (_,IWildCard,t) -> (TWildCard,t) | (_,IAdd,t) -> (TAdd,t) 
	| (_,ISub,t) -> (TSub,t) | (_,IMult,t) -> (TMult,t)
	| (_,IDiv,t) -> (TDiv,t) | (_,IMod,t) -> (TMod,t) | (_,IPow,t) -> (TPow,t) 
	| (_,IAddF,t) -> (TAddF,t) | (_,ISubF,t) -> (TSubF,t) 
	| (_,IMultF,t) -> (TMultF,t) | (_,IDivF,t) -> (TDivF,t) 
	| (_,IPowF,t) -> (TPowF,t)
	| (_,INeg,t) -> (TNeg,t) | (_,IEq,t) -> (TEq,t) | (_,IEqF,t) -> (TEqF,t) 
	| (_,INeq,t) -> (TNeq,t) | (_,INeqF,t) -> (TNeqF,t)
	| (_,IGeq,t) -> (TGeq,t) | (_,IGeqF,t) -> (TGeqF,t) 
	| (_,ILeq,t) -> (TLeq,t) | (_,ILeqF,t) -> (TLeqF,t) 
	| (_,ILess,t) -> (TLess,t) | (_,ILessF,t) -> (TLessF,t) 
	| (_,IGreater,t) -> (TGreater,t) | (_,IGreaterF,t) -> (TGreaterF,t) 
	| (_,IAnd,t) -> (TAnd,t) | (_,IOr,t) -> (TOr,t) | (_,INot,t) -> (TNot,t)
    | (_,ICons,t) -> (TCons,t) | (_,ICat,t) -> (TCat,t) 
	| (_,ILen,t) -> (TLen,t) | (_,IHead,t) -> (THead,t) 
	| (_,ITail,t) -> (TTail,t) 
	| (_,IVar (str),t) -> (TVar str,t)
    | (_,ILet (_, iassign, ix1),t) ->
		(TLet(remove_subst_iassign iassign, remove_subst_expr ix1),t)
    | (_,ILambda(_, var, ix2),t) ->
		(TLambda(var, remove_subst_expr ix2),t)
    | (_,IApp(_, ix1, ix2),t) ->
		(TApp(remove_subst_expr ix1, remove_subst_expr ix2),t)
    | (_,IIte(_,ix1, ix2, ix3),t) ->
		(TIte(remove_subst_expr ix1, remove_subst_expr ix2, 
			remove_subst_expr ix3),t)
	| (_,IListComp(_,ix1, iclause_list),t) ->
		(TListComp(remove_subst_expr ix1,
		List.map remove_subst_clause iclause_list),t)
	| (_,IListRange(_,ix1,ix2),t) ->
		(TListRange(remove_subst_expr ix1, remove_subst_expr ix2),t)
	| (_,IInfList(_,ix1),t) ->
		(TInfList (remove_subst_expr ix1),t)
    | (_,(IListLit(ix_list)),t) ->
		(TListLit (List.map remove_subst_expr ix_list),t)
and remove_subst_clause = function
	| IListVBind(str,ix2) ->
		TListVBind(str, remove_subst_expr ix2)	
	| IFilter ix1 ->
		TFilter (remove_subst_expr ix1)
and remove_subst_iassign = function
	| IAssign(str,ix2) ->
		TAssign(str, remove_subst_expr ix2)

let remove_subst = function
	|InferredVdef(name,expr) ->	TypedVdef(name, remove_subst_expr expr)

let rec remove_subst_pairs = function
	| [] -> []
	| (annot, infvdef)::xs -> 
		(annot, remove_subst infvdef)::(remove_subst_pairs
xs)
