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

let printEnv env =
	print_string "[";
	TyEnvMap.iter (fun key -> fun ty -> 
		print_string (key ^ " :: " ^ (ty_to_str ty)^", ")) env;
	print_endline "]"


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
    | Tvar(n) -> 
		(match SubstMap.find_opt n s with
        | Some t -> (*(print_endline ("APPLY LOOKUP: "^n^":"^(ty_to_str t)));*)t
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

let printSubst s = print_string "{" ;
					SubstMap.iter 
					(fun key -> fun ty ->
					print_string (key ^ ": " ^ (ty_to_str ty)^", ")) s;
					print_endline "}"

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
    | u, Tvar(x) -> if (String.equal u x) 
					then nullSubst 
					else SubstMap.add u (Tvar(x)) SubstMap.empty
    | u, t when SS.mem u (ftv t) -> raise
		(Failure ("Cannot bind "^u^" to "^(ty_to_str t)) )
    | _,_ -> SubstMap.add u t SubstMap.empty

let rec mgu ty1 ty2 = 
	match ty1, ty2 with
    | Tarrow(l, r), Tarrow(l', r') -> 
            let s1 = mgu l l' in
(*			print_string "MGU s1: ";
			printSubst s1; *)
            let s2 = mgu (apply s1 r) (apply s1 r') in
(*			print_endline("MGU apply r1: " ^ (ty_to_str (apply s1 r)));
			print_endline("MGU apply r2: " ^ (ty_to_str (apply s1 r')));
			print_endline("MGU r1: " ^ (ty_to_str r));
			print_endline("MGU r2: "^ (ty_to_str r')); 
 			print_string "MGU s2: ";
			printSubst s2;       
			print_string "MGU ret subst: ";
			printSubst (composeSubst s1 s2);    *)
			composeSubst s1 s2
    | Tvar(u), t -> varBind u t
    | t, Tvar(u) -> varBind u t
    | Int, Int -> nullSubst
    | Bool, Bool -> nullSubst
	(* Lol does this work *)
	| Float, Int -> nullSubst 
	| Int, Float -> nullSubst
	(* probably not *)
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
let rec ti env expr =
	let rec ti_vbinds env = function
		| ((ListVBind(v,e))::xs) -> 
			let (s,ix,ty) as ixpr = 
			(match e with 
			| (ListComp _) as l -> ti env l
			| (ListRange _) as l -> ti env l
			| (InfList _) as l -> ti env l
			| (ListLit _) as l -> ti env l
			| _ -> raise (Failure( 
			"list comprehension variable "^v^" is not defined over a list"))) 
			in (IListVBind(v,ixpr))::(ti_vbinds (applyenv s env) xs)	
		| [] -> []
	in
	let rec ti_filters env = function
		| ((Filter e)::xs) -> 
			let (s,ix,ty) as ixpr = ti env e in
			(IFilter ixpr)::(ti_filters (applyenv s env) xs)
		| [] -> []
	in
	let collect_vbinds_filters mixed_list =
		let rec collect l tuple =
			match tuple with (vbinds,filters) ->
			(match l with
			| (ListVBind(t)::xs) -> collect xs ((ListVBind(t))::vbinds,filters)
			| (Filter(t) ::xs) -> collect xs (vbinds,(Filter(t))::filters)
			| [] -> (List.rev vbinds, List.rev filters))
		in collect mixed_list ([],[]) 
	in
	let rec tys_from_vbinds = function
		| (IListVBind(_,(_,_,(TconList ty))))::xs -> ty::(tys_from_vbinds xs)
		| [] -> []
		| _ -> raise 
			(Failure "list comprehension variable is not defined over list")
	in
	let rec tys_from_filters = function
		| (IFilter(_,_,ty))::xs -> ty::(tys_from_filters xs)
		| [] -> []
		| _ -> raise (Failure "error in tys_from_filters")
	in
	let rec substs_from_vbinds = function
		| (IListVBind(_,(s,_,_)))::xs -> composeSubst s (substs_from_vbinds xs)
		| [] -> nullSubst
		| _ -> raise 
			(Failure "list comprehension variable is not defined over list")
	in 
	let rec substs_from_filters = function
		| (IFilter(s,_,_))::xs -> composeSubst s (substs_from_filters xs)
		| [] -> nullSubst
		| _ -> raise (Failure "error in tys_from_filters")

	in
	let rec merge_tys_filter tlist filter_ty = match tlist with
		| (ty::xs) -> (match filter_ty with
			| Tarrow(arg,ret) -> 
				let s1 = mgu arg ty in
				let s2 = merge_tys_filter xs ret in
				composeSubst s1 s2
			| _ -> raise (Failure "improper filter type in list comprehension"))
		| [] -> (mgu filter_ty Bool)
	in
	let rec merge_tys_expr tlist expr_ty = match tlist with
		|(ty::xs) -> (match expr_ty with
			| Tarrow(arg,ret) ->
				let s1 = mgu arg ty in
				let (s2,ret_ty) = merge_tys_expr xs ret in
				let s3 = composeSubst s1 s2 in
				(s3, apply s3 ret_ty)
			| _ -> raise (Failure "improper variable bindings in list comp"))
		| [] -> (nullSubst,expr_ty)
	in
	let type_listcomp env comp = match comp with (ListComp(e,clauses)) ->
		let (s,ix,ty) as ixpr = ti env e in
		let (vbinds,filters) = collect_vbinds_filters clauses in
		let (ivbinds,ifilters) =(ti_vbinds env vbinds,ti_filters env filters) in
		let vbind_tys = tys_from_vbinds ivbinds in
		let vsubsts = substs_from_vbinds ivbinds in
		let fsubsts = substs_from_filters ifilters in 
		let filter_tys = tys_from_filters ifilters in 
		let filtsubst = composeSubst fsubsts (List.fold_left 
			(fun su -> fun t -> composeSubst (merge_tys_filter vbind_tys t) su)
			nullSubst filter_tys) in
		let (esubst,ety) = merge_tys_expr vbind_tys ty in
		let allsubst = composeSubst vsubsts (composeSubst filtsubst esubst) in
(*		print_string "COMP allsubst: "; printSubst allsubst;*)
		let ret_ty = apply allsubst ety in 
		(* should we apply substs to the inferred vbinds and filters? prob ye *)
		(allsubst,IListComp(allsubst,ixpr,(ivbinds@ifilters)), TconList(ret_ty))
	in
	(****************************EXPRS******************************) 
	match expr with
    | IntLit i -> (nullSubst, IIntLit i, Int)
    | FloatLit f -> (nullSubst, IFloatLit f,Float)
    | CharLit c -> (nullSubst, ICharLit c,Char)
    | BoolLit b -> (nullSubst, IBoolLit b,Bool)
	| Tuple (e1,e2) -> 
		let (s1,tex1,ty1) as ix1 = ti env e1 in
		let (s2,tex2,ty2) as ix2 = ti (applyenv s1 env) e2 in
		let s3 = composeSubst s1 s2 in
		(s3
		, ITuple(ix1,ix2)
		, TconTuple(apply s3 ty1, apply s3 ty2))
    | ListLit [] -> (nullSubst, IListLit [], TconList(newTyVar "a"))
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
(*		print_string "RANGE subst1: ";
		printSubst subst1;*)
		let (subst2,tex2, ty2) = ti (applyenv subst1 env) e2 in	
(*		print_string "RANGE subst2: ";
		printSubst subst2;*)
		let subst3 = mgu (apply subst2 ty1) ty2 in
(*		print_string "RANGE subst3: ";
		printSubst subst3;*)
		let subst4 = mgu (apply subst3 ty2) Int in
(*		print_string "RANGE subst4: ";
		printSubst subst4;*)
		let fullsubst = composeSubst subst1 (composeSubst subst2 
			(composeSubst subst3 subst4)) in
(*		print_string "RANGE fullsubst: ";
		printSubst fullsubst;
		print_endline ("RANGE ty1: "^(ty_to_str (apply fullsubst ty1)));
		print_endline ("RANGE ty2: "^(ty_to_str (apply fullsubst ty2)));*)
		(fullsubst
			, IListRange(subst4, 
				(subst1,tex1,apply fullsubst ty1), 
				(subst2,tex2,apply fullsubst ty2))
			, TconList Int)
	| InfList e ->
		let (subst, tex, ty) = ti env e in
		let subst' = mgu (apply subst ty) Int in
		(subst', IInfList(subst', (subst,tex,ty)), TconList Int)
	| None -> let polyty = newTyVar "a" in
		(nullSubst, INone, Tmaybe polyty)
	| Just e -> let (s,ix,t) as ixpr = ti env e in
		(s, IJust ixpr, Tmaybe t)
	| Tuple (e1,e2) -> 
		let (s1,ix1,t1) as ixpr1 = ti env e1 in
		let (s2,ix2,t2) as ixpr2 = ti (applyenv s1 env) e2 in
		let fullSubst = composeSubst s1 s2 in
		(fullSubst, ITuple(ixpr1,ixpr2), 
			TconTuple(apply fullSubst t1, apply fullSubst t2))
	| ListComp(_) as comp -> type_listcomp env comp 
        | Var n -> let sigma = TyEnvMap.find_opt n env in 
                (match sigma with
                | None -> raise(Failure("unbound variable" ^ n))
                | Some si -> let t = instantiate si in 
					(nullSubst, IVar n, t)
                )
        | Let(Assign(x, e1), e2) -> 
				let (s1,tex1,t1) as ix1 = ti env e1 in
(*				(print_endline ("let assign ty: "^ (ty_to_str t1))); *)
                let t' = generalize (applyenv s1 env) t1 in 
                let env'' = (TyEnvMap.add x t' (applyenv s1 env)) in 
                let (s2, tex2, t2) as ix2 = ti (applyenv s1 env'') e2 in
                (composeSubst s1 s2
				, ILet(composeSubst s1 s2, IAssign(x, ix1), ix2)
				, t2)
	| Lambda( n, e ) -> 
		let tv = newTyVar n in 
        let env' = remove env n in 
        let env'' = SubstMap.union collision env' 
			(SubstMap.singleton n (Tforall([], tv)) ) in
        let (s1, tex1, t1) as ix1 = ti env'' e in
        (s1, ILambda (s1, n, ix1), Tarrow( (apply s1 tv), t1 ))
	| App(e1,e2) -> 
		let tv = newTyVar "app" in
		let (s1, tx1, t1) as ix1 = ti env e1 in
(*		print_endline ("APP t1: " ^ (ty_to_str t1));*)
		let (s2, tx2, t2) as ix2 = ti (applyenv s1 env) e2 in
		let s3 = mgu (apply s2 t1) (Tarrow( t2, tv)) in
(*		print_endline ("APP t2: " ^ (ty_to_str t2));
		print_string "APP s3: ";
		printSubst s3;
		print_string "APP ret subst: ";
		printSubst (composeSubst (composeSubst s1 s2) s3);
		print_endline ("APP ret ty: "^(ty_to_str(apply s3 tv)));*)
		((composeSubst (composeSubst s1 s2) s3)
		, IApp(s3,ix1,ix2)
		, apply s3 tv)
	| Ite(e1,e2,e3) ->
		let (s1,tx1,t1) as ix1 = ti env e1 in
		(*first expr must be boolean*)
		let boolSubst = composeSubst (mgu Bool t1) s1 in 
		let (s2,tx2,t2) as ix2 = ti (applyenv boolSubst env) e2 in
		let s' = composeSubst boolSubst s2 in
		let (s3,tx2,t3) as ix3 = ti (applyenv s' env) e3 in
		let s'' = mgu t2 t3 in
		let fullSubst = composeSubst s' s'' in
		(fullSubst
		, IIte(fullSubst, ix1,ix2,ix3)
		, apply fullSubst t2)
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
		| Cons -> let polyty = newTyVar "a" in
			(nullSubst, ICons, Tarrow(polyty, 
			Tarrow (TconList polyty, TconList polyty)))
		| Cat -> let polyty = newTyVar "a" in
			(nullSubst, ICat, Tarrow(TconList polyty, 
				Tarrow (TconList polyty, TconList polyty)))
		| Len -> let polyty = newTyVar "a" in
			(nullSubst, ILen, Tarrow(TconList polyty, Int))
		| Head -> let polyty = newTyVar "a" in
			(nullSubst, IHead,
			(Tarrow(TconList polyty, polyty)))
		| Tail -> let polyty = newTyVar "a" in
			(nullSubst, ITail, 
			Tarrow(TconList polyty, TconList polyty))
		| First -> let polyty1 = newTyVar "a" in
					let polyty2 = newTyVar "b" in
					(nullSubst, IFirst,
					(Tarrow(TconTuple(polyty1,polyty2),polyty1)))
		| Sec -> let polyty1 = newTyVar "a" in
					let polyty2 = newTyVar "b" in
					(nullSubst, ISec,
					(Tarrow(TconTuple(polyty1,polyty2),polyty2)))
        (* TODO: rest of add things *)
        | _ -> raise (Failure "not yet implemented in type inference") 

(*let rec type_clauses env = function
        (* make sure that var is the same type as blist *)
	| ListVBind (var, blist) -> let (subst, tex, ty) = ti env blist
	| Filter e -> let (subst, tex, ty) = ti env e in 
        let subst' = mgu (apply subst ty) Bool in
        (subst', IFilter(subst', tex, apply subst' ty), apply subst' ty)*)

let rec typeUpdateEnv env = function
	| ((a,Vdef(name,expr))::xs) ->
		let (substs, ix, ty) = ti env expr in
		let newTy = generalize env ty in
		let oldTy = 
                (match TyEnvMap.find_opt name env with
                | None -> raise(Failure("unbound variable " ^ name))
                | Some si -> instantiate si) in
		let newSubst = mgu newTy oldTy in
		let newPair = (a, InferredVdef(name,
			(composeSubst newSubst substs, ix,ty))) in
		newPair::(typeUpdateEnv (applyenv newSubst env) xs)
	| [] -> []
	| ((_,Annot(_))::xs) -> raise (Failure "cannot tiVdef on annotation")

let rec unzip_thruple l =
	let f (l1,l2,l3) (x,y,z) = (x::l1,y::l2,z::l3) in
	List.fold_left f ([],[],[]) (List.rev l)

let type_paired_program annotvdef_list =
	let vdef_names = List.fold_left 
		(fun l -> fun ((Annot(n,_)),_) -> n::l) 
		[] annotvdef_list in
	let moduleEnv = List.fold_left 
		(fun env -> fun name -> 
			let var = newTyVar name in
			TyEnvMap.add name var env) 
		TyEnvMap.empty vdef_names in

	let annotIVdefs = typeUpdateEnv moduleEnv annotvdef_list in

	let substList = List.fold_left
		(fun l -> fun (_, InferredVdef(_,(subst,_,_))) -> subst::l)
		[] annotIVdefs in
	let allSubsts = List.fold_left
		(fun s1 -> fun s2 -> composeSubst s1 s2)
		(List.hd substList) substList in
	let annotIVdefs' = List.map
		(fun (a, InferredVdef(n,(s,ix,ty))) -> 
			(a,InferredVdef(n,(s,ix, apply allSubsts ty)))) annotIVdefs in
		annotIVdefs'

