open Ast
open Tast
open Get_fresh_var
open Iast
open List
open Pretty_type_print

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
    | Tarrow (t1, t2) -> Tarrow ( apply  s t1, apply s t2 )
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

let instantiate = function
    | Tforall(vars, t) -> 
		let nvars = List.map (fun var -> newTyVar(var)) vars in 
		let s = map_from_list (zip vars nvars) in 
		apply s t
    | t -> t

let varBind u t = match u, t with
    | u, Tvar(x) -> nullSubst
    | u, t when SS.mem u (ftv t) -> raise(Failure("Occur check fails "))
    | _,_ -> SubstMap.add u t SubstMap.empty

let rec mgu ty1 ty2 = 
	match ty1, ty2 with
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
    | t1, t2 -> raise(Failure ((ty_to_str ty1) ^ " types do not unify " ^
(ty_to_str ty2)))



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
let rec ti env = function
    | IntLit i -> (nullSubst, IIntLit i, Int)
    | FloatLit f -> (nullSubst, IFloatLit f,Float)
    | CharLit c -> (nullSubst, ICharLit c,Char)
    | BoolLit b -> (nullSubst, IBoolLit b,Bool)
    | ListLit l -> let iexpr_list = List.map (ti env) l in
		(match iexpr_list with
		(* collect all substs; apply substs on elements and final type *)
		| ix_list -> 
			let fullSubst = 
			fold_left (fun s1 (s2,_,_) -> composeSubst s1 s2) env ix_list in
			let merged_ix_list = List.map 
				(fun (env,e,t) -> (env,e, apply fullSubst t)) ix_list in
			let (_,_,ty) = List.hd merged_ix_list in
			(fullSubst, IListLit(merged_ix_list), TconList ty)
		| [] -> (nullSubst, IListLit [], TconList (newTyVar "a")))
	| ListRange(e1, e2) -> 
		let (subst1, tex1, ty1) = ti env e1 in
		let (subst2,tex2, ty2) = ti (applyenv subst1 env) e2 in
		let subst3 = mgu (apply subst2 ty1) ty2 in
		let subst4 = mgu (apply subst3 ty1) Int in
		(subst4, IListRange(subst4, (subst1,tex1,ty1), (subst2,tex2,ty2)), 
			TconList Int)
	| InfList e ->
		let (subst, tex, ty) = ti env e in
		let subst' = mgu (apply subst ty) Int in
		(subst', IInfList(subst', (subst,tex,ty)), TconList Int)
	(*| ListComp(e, clauses) ->*)
        | Var n -> let sigma = TyEnvMap.find_opt n env in 
                (match sigma with
                | None -> raise(Failure("unbound variable" ^ n))
                | Some si -> let t = instantiate si in (nullSubst, IVar n, t)
                )
        | Let(Assign(x, e1), e2) -> let (s1,tex1,t1) as ix1 = ti env e1 in 
                let env' = remove env x in
                let t' = generalize (applyenv s1 env) t1 in 
                let env'' = (TyEnvMap.add x t' env') in 
                let (s2, tex2, t2) as ix2 = ti (applyenv s1 env'') e2 in
                (composeSubst s1 s2, ILet(composeSubst s1 s2, IAssign(x, ix1), ix2), t2)
	| Lambda( n, e ) -> 
		let tv = newTyVar n in 
        let env' = remove env n in 
        let env'' = SubstMap.union collision env' 
			(SubstMap.singleton n (Tforall([], tv)) ) in
        let (s1, tex1, t1) as ix1 = ti env'' e in 
        (s1, ILambda (s1, n, ix1), Tarrow( (apply s1 tv), t1 ))
	| App(e1,e2) -> 
		let tv = newTyVar "a" in
		let (s1, tx1, t1) as ix1 = ti env e1 in
		let (s2, tx2, t2) as ix2 = ti (applyenv s1 env) e2 in
		let s3 = mgu (apply s2 t1) (Tarrow (t2, tv)) in
		((composeSubst (composeSubst s1 s2) s3)
		, IApp(s3,ix1,ix2)
		, apply s3 tv)
	| Add -> (nullSubst, IAdd, Tarrow(Int, Tarrow(Int,Int)))
        | Sub -> (nullSubst, ISub, Tarrow(Int, Tarrow(Int,Int)))
        | Mult -> (nullSubst, IMult, Tarrow(Int, Tarrow(Int,Int)))
        | Div -> (nullSubst, IDiv, Tarrow(Int, Tarrow(Int,Int)))
        | Mod -> (nullSubst, IMod, Tarrow(Int,Tarrow(Int,Int)))
        | Pow -> (nullSubst, IPow, Tarrow(Int,Tarrow(Int,Int)))
        | AddF -> (nullSubst, IAddF, Tarrow(Float, Tarrow(Float,Float)))
        | SubF -> (nullSubst, ISubF, Tarrow(Float, Tarrow(Float,Float)))
        | MultF -> (nullSubst, IMultF, Tarrow(Float, Tarrow(Float,Float)))
        | DivF -> (nullSubst, IDivF, Tarrow(Float,Tarrow(Float,Float)))
        | PowF -> (nullSubst, IPowF, Tarrow(Float,Tarrow(Float,Float)))
        | Neg -> (nullSubst, INeg, Tarrow(Int, Int))
        | Eq -> (nullSubst, IEq, Tarrow(Int,Tarrow(Int,Bool)))
        | EqF -> (nullSubst, IEqF, Tarrow(Float,Tarrow(Float,Bool)))
        | Neq -> (nullSubst, INeq, Tarrow(Int,Tarrow(Int,Bool)))
        | NeqF -> (nullSubst, INeqF, Tarrow(Float,Tarrow(Float,Bool)))
        | Geq -> (nullSubst, IGeq, Tarrow(Int,Tarrow(Int,Bool)))
        | GeqF -> (nullSubst, IGeqF, Tarrow(Float,Tarrow(Float,Bool)))
        | Leq -> (nullSubst, ILeq, Tarrow(Int,Tarrow(Int,Bool)))
        | LeqF -> (nullSubst, ILeqF, Tarrow(Float,Tarrow(Float,Bool)))
        | Less -> (nullSubst, ILess, Tarrow(Int,Tarrow(Int,Bool)))
        | LessF -> (nullSubst, ILessF, Tarrow(Float,Tarrow(Float,Bool)))
        | Greater -> (nullSubst, IGreater, Tarrow(Int,Tarrow(Int,Bool)))
        | GreaterF -> (nullSubst, IGreaterF, Tarrow(Float,Tarrow(Float,Bool)))
        | And -> (nullSubst, IAnd, Tarrow(Bool,Tarrow(Bool,Bool)))
        | Or -> (nullSubst, IOr, Tarrow(Bool,Tarrow(Bool,Bool)))
        | Not -> (nullSubst, INot, Tarrow(Bool,Bool))  

        (* TODO: rest of add things *)
        | _ -> raise (Failure "not yet implemented in type inference") 
(*let rec type_clauses env = function
	| ListVBind (var, blist) ->
	| Filter e ->*)

let rec type_paired_program = function
	| ((annot, (Vdef (name,exp)))::xs) -> 
		let iexpr = ti TyEnvMap.empty exp in
		let tpair = (annot, (InferredVdef (name, iexpr) ) ) in
		tpair :: (type_paired_program xs)
        | ((_, Annot _) :: _) -> raise(Failure("no"))
	| [] -> []
