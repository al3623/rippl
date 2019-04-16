open Ast
open Tast
open Get_fresh_var
open Iast
open List

module SS = Set.Make(String);;
module SMap = Map.Make(String);;
module SubstMap = Map.Make(String);;

(* mappings from term variables to tforall *)
module TyEnvMap = Map.Make(String);;

(* returns a set of free type variables *)
let rec ftv = function
    | Tvar(n) -> SS.add n SS.empty
    | Int -> SS.empty
    | Bool -> SS.empty
    | Float -> SS.empty
    | Char -> SS.empty
    | Tarrow (t1, t2) -> SS.union (ftv t1) (ftv t2)
    | TconList (t) -> ftv t
    | TconTuple (t1, t2) -> SS.union (ftv t1) (ftv t2)
    | Tforall (stlst, t) -> SS.diff (ftv t) (SS.of_list stlst)
    | Tmaybe (t) -> ftv t

let rec apply s = function
    | Tvar(n) -> (match SubstMap.find_opt n s with
        | Some t -> t
        | None -> Tvar(n)
    )
    | Tarrow (t1, t2) -> Tarrow ( apply  s t2, apply s t2 )
    | TconList (t) -> TconList (apply s t)
    | TconTuple (t1, t2) -> TconTuple (apply s t1, apply s t2)
    | Tforall (stlst, t) -> Tforall (stlst, 
		apply (List.fold_right SubstMap.remove stlst s) t)
    | Tmaybe(t) -> Tmaybe(apply s t) 
    | t -> t 

let collision key e1 e2 = Some e1

let nullSubst : ty SubstMap.t = SubstMap.empty

let composeSubst (s1 : ty SubstMap.t) (s2 : ty SubstMap.t) = 
    SubstMap.union collision (SubstMap.map (apply s1) s2) s1

(* removes element from typing environment *)
let remove (env : ty SubstMap.t) var =
    SubstMap.remove var env

let getElem = function
    | (key, a) -> a

let getElems mp = List.map getElem (TyEnvMap.bindings mp)



(*	get elements of the map (not the keys), 
	and map ftv over them then make a new set with those ftvs*)
let ftvenv env = 
	(List.fold_right ( SS.union ) (List.map ftv (getElems env)) SS.empty )

let applyenv subst env = (TyEnvMap.map (apply subst) env)

let generalize env t = 
    let vars = SS.elements (SS.diff (ftv t) (ftvenv env)) in Tforall(vars, t)

let newTyVar prefix = 
    let str = get_fresh prefix in Tvar(str)

let rec zip lst1 lst2 = match lst1, lst2 with
    | [], _ -> []
    | _, [] -> []
    | (x :: xs), (y :: ys) -> (x, y) :: (zip xs ys)


let rec map_from_list = function 
    | [] -> SubstMap.empty
    | (t1, t2) :: tl -> SubstMap.add t1 t2 (map_from_list tl)

let instantiate tval = function
    | Tforall(vars, t) -> 
		let nvars = List.map (fun var -> newTyVar(var)) vars in 
		let s = map_from_list (zip vars nvars) in 
		apply s t
    | t -> t

let varBind u t = match u, t with
    | u, Tvar(x) -> nullSubst
    | u, t when SS.mem u (ftv t) -> raise(Failure("Occur check fails "))
    | _,_ -> SubstMap.add u t SubstMap.empty

let rec mgu ty1 ty2 = match ty1, ty2 with
    | Tarrow(l, r), Tarrow(l', r') -> 
            let s1 = mgu l l' in 
            let s2 = mgu (apply s1 r) (apply s1 r') in 
            composeSubst s1 s2
    | Tvar(u), t -> varBind u t
    | t, Tvar(u) -> varBind u t
    | Int, Int -> nullSubst
    | Bool, Bool -> nullSubst
    | Float, Float -> nullSubst
    | Char, Char -> nullSubst
    | TconList(t), TconList(t') -> mgu t t'
    | Tmaybe(t), Tmaybe(t') -> mgu t t'
    | TconTuple(l, r), TconTuple(l', r') -> 
            let s1 = mgu l l' in
            let s2 = mgu (apply s1 r) (apply s1 r') in 
            composeSubst s1 s2
    | t1, t2 -> raise(Failure (" types do not unify "))



(* Collects tvars in a list; doesn't work for tforalls because we 
 * shouldn't need to call it on a tforall *)
let collect_tvar =
    let rec collect genlist = function
        | (Tvar var) -> var::genlist
        | (TconList ty) -> (collect genlist ty)
        | (TconTuple (t1,t2)) -> let l1 = collect genlist t1 in
            let l2 = collect l1 t2 in l2
        | (Tarrow (t1,t2)) -> let l1 = collect genlist t1 in
            let l2 = collect l1 t2 in l2
        | (Tmaybe ty) -> (collect genlist ty)
        | (Tforall _) -> raise (Failure "can't generalize tforalls")
        | _ -> genlist
    in collect []

(* Returns tforall if there are tvars, normal type if not *)
let simple_generalize ty =
    let gen_list = collect_tvar ty in
    if (List.length gen_list) = 0 then ty else (Tforall (gen_list,ty))

(* Takes an AST and returns a TAST (typed AST) *)
let rec ti s = function
    | IntLit i -> (nullSubst, IIntLit i, Int)
    | FloatLit f -> (nullSubst, IFloatLit f,Float)
    | CharLit c -> (nullSubst, ICharLit c,Char)
    | BoolLit b -> (nullSubst, IBoolLit b,Bool)
    | ListLit l -> let iexpr_list = List.map (ti s) l in
		(match iexpr_list with
		(* collect all substs; apply substs on elements and final type *)
		| ix_list -> 
			let fullSubst = 
			fold_left (fun s1 (s2,_,_) -> composeSubst s1 s2) s ix_list in
			let merged_ix_list = List.map 
				(fun (s,e,t) -> (s,e, apply fullSubst t)) ix_list in
			let (_,_,ty) = List.hd merged_ix_list in
			(fullSubst, IListLit(merged_ix_list), TconList ty)
		| [] -> (nullSubst, IListLit [], TconList (newTyVar "a")))
(*  | Lambda( n, e ) -> let tv = newTyVar n in 
        let env' = remove env n in 
        let env'' = SubstMap.union env' (Map.singleton (n Tforall([], tv))) in
        let (s1, t1) = ti env'' e in 
        (s1, TLambda (n, e), TArrow( (apply s1 tv), t1 )) *)
	| Lambda(_) -> raise (Failure "NO LAMBDAS :(")
    | _ -> raise (Failure "not yet implemented in type inference") 

let rec type_paired_program = function
	| ((annot, (Vdef (name,exp)))::xs) -> 
		let iexpr = ti TyEnvMap.empty exp in
		let tpair = (annot, (InferredVdef (name, iexpr) ) ) in
		tpair :: (type_paired_program xs)
        | ((_, Annot _) :: _) -> raise(Failure("no"))
	| [] -> []
