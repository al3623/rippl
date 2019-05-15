open Iast
open Tast
open Type_inference

let rec remove_subst_expr subst = function
	| (_,IIntLit i,t) -> (TIntLit i,t)
	| (_,IFloatLit f,t) -> (TFloatLit f, t)
	| (_,IBoolLit b,t) -> (TBoolLit b,t)
    | (_,ICharLit c,t) -> (TCharLit c,t)
	| (_,ITuple(ix1,ix2),t) -> 
		(TTuple(remove_subst_expr subst ix1, remove_subst_expr subst ix2), apply subst t)
	| (_,IWildCard,t) -> (TWildCard,apply subst t) | (_,IAdd,t) -> (TAdd,t) 
	| (_,ISub,t) -> (TSub,t) | (_,IMult,t) -> (TMult,t)
	| (_,IDiv,t) -> (TDiv,t) | (_,IMod,t) -> (TMod,t) | (_,IPow,t) -> (TPow,t) 
	| (_,IAddF,t) -> (TAddF,t) | (_,ISubF,t) -> (TSubF,t) 
	| (_,IMultF,t) -> (TMultF,t) | (_,IDivF,t) -> (TDivF,t) 
	| (_,IPowF,t) -> (TPowF,t) | (_,INegF,t) -> (TNegF,t) 
	| (_,INeg,t) -> (TNeg,t) | (_,IEq,t) -> (TEq,t) | (_,IEqF,t) -> (TEqF,t) 
	| (_,INeq,t) -> (TNeq,t) | (_,INeqF,t) -> (TNeqF,t)
	| (_,IGeq,t) -> (TGeq,t) | (_,IGeqF,t) -> (TGeqF,t) 
	| (_,ILeq,t) -> (TLeq,t) | (_,ILeqF,t) -> (TLeqF,t) 
	| (_,ILess,t) -> (TLess,t) | (_,ILessF,t) -> (TLessF,t) 
	| (_,IGreater,t) -> (TGreater,t) | (_,IGreaterF,t) -> (TGreaterF,t) 
	| (_,IAnd,t) -> (TAnd,t) | (_,IOr,t) -> (TOr,t) | (_,INot,t) -> (TNot,t)
    | (_,ICons,t) -> (TCons,apply subst t) | (_,ICat,t) -> (TCat,apply subst t) 
	| (_,ILen,t) -> (TLen,apply subst t) | (_,IHead,t) -> (THead,apply subst t) 
	| (_,ITail,t) -> (TTail,apply subst t)
	| (_,IInt_to_float,t) -> (TInt_to_float,t) 
	| (_,IVar (str),t) -> (TVar str,apply subst t)
    | (_,ILet (_, iassign, ix1),t) ->
		(TLet(remove_subst_iassign subst iassign, remove_subst_expr subst ix1),apply subst t)
    | (_,ILambda(_, var, ix2),t) ->
		(TLambda(var, remove_subst_expr subst ix2),apply subst t)
    | (_,IApp(_, ix1, ix2),t) ->
		(TApp(remove_subst_expr subst ix1, remove_subst_expr subst ix2),apply subst t)
    | (_,IIte(_,ix1, ix2, ix3),t) ->
		(TIte(remove_subst_expr subst ix1, remove_subst_expr subst ix2, 
			remove_subst_expr subst ix3),apply subst t)
	| (_,IListComp(_,ix1, iclause_list),t) ->
		(TListComp(remove_subst_expr subst ix1,
		List.map (remove_subst_clause subst) iclause_list),apply subst t)
	| (_,IListRange(_,ix1,ix2),t) ->
		(TListRange(remove_subst_expr subst ix1, remove_subst_expr subst ix2),apply subst t)
    | (_,(IListLit(ix_list)),t) ->
		(TListLit (List.map (remove_subst_expr subst) ix_list),apply subst t)
	| (_,(IFirst),t) -> (TFirst,apply subst t) 
	| (_,(ISec),t) -> (TSec,apply subst t)
	| (_,(IJust ix),t) -> (TJust(remove_subst_expr subst ix),apply subst t)
	| (_,(INone),t) -> (TNone,apply subst t)
	| (_,(IIs_none),t) -> (TIs_none,apply subst t)
	| (_,IFrom_just,t) -> (TFrom_just,apply subst t)
and remove_subst_clause subst = function
	| IListVBind(str,ix2) ->
		TListVBind(str, remove_subst_expr subst ix2)	
	| IFilter ix1 ->
		TFilter (remove_subst_expr subst ix1)
and remove_subst_iassign subst = function
	| IAssign(str,ix2) ->
		TAssign(str, remove_subst_expr subst ix2)

let remove_subst subst = function
	|InferredVdef(name,expr) ->	TypedVdef(name, remove_subst_expr subst expr)

let rec remove_subst_pairs subst = function
	| [] -> []
	| (annot, infvdef)::xs -> 
		(annot, remove_subst subst infvdef)::(remove_subst_pairs subst xs)
