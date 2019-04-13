open Ast
open Tast

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
    | Tforall (stlst, t) -> Tforall (stlst, apply (List.fold_right SubstMap.remove stlst s) t)
    | Tmaybe(t) -> Tmaybe(apply s t) 
    | t -> t 

let collision key e1 e2 = Some e1

let nullSubst : ty SubstMap.t = SubstMap.empty

let composeSubst (s1 : ty SubstMap.t) (s2 : ty SubstMap.t) = 
    SubstMap.union collision (SubstMap.map (apply s1) s2) s1

(* removes element from typing environment *)
let remove (env : ty SubstMap.t) var =
    TyEnvMap.remove var env

let getElem = function
    | (key, a) -> a

let getElems mp = List.map getElem (TyEnvMap.bindings mp)



(* get elements of the map (not the keys), and map ftv over them then make a new set with those ftvs*)
let ftvenv env = (List.fold_right ( SS.union ) (List.map ftv (getElems env)) SS.empty )

let applyenv subst env = (TyEnvMap.map (apply subst) env)

let generalize env t = 
    let vars = SS.elements (SS.diff (ftv t) (ftvenv env)) in Tforall(vars, t)




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
let rec infer_type = function
    | IntLit i -> (TIntLit i, Int)
    | FloatLit f -> (TFloatLit f,Float)
    | CharLit c -> (TCharLit c,Char)
    | BoolLit b -> (TBoolLit b,Bool)
    | ListLit l -> ( match l with
        | x::xs -> let (texpr,typ) = infer_type x in
            (TListLit (List.map infer_type (x::xs)), TconList typ)
        | [] ->  (TListLit [], (Tvar "a")))
    | Lambda( e1, e2) -> infer_type e2 
    | _ -> raise (Failure "not yet implemented in type inference")

let rec type_paired_program = function
	| ((annot, (Vdef (name,exp)))::xs) -> let (texpr, infty) = infer_type exp in
		let tpair = (annot, (TypedVdef (name, (texpr,infty) ) ) ) in
		tpair :: (type_paired_program xs)
        | ((_, Annot _) :: _) -> raise(Failure("no"))
	| [] -> []
