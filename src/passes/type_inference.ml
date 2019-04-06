open Ast
open Tast


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
	| ListRange (IntLit s, IntLit f) ->
		(TListRange ((TIntLit s,Int), (TIntLit f,Int)), TconList Int)
    | _ -> raise (Failure ":C")

let catch_main = function
	| (annot, Vdef (name, exp)) -> (if name = "main" 
		then let new_expr = (match exp with
		| Lambda (_,ret_expr) -> ret_expr
		| _ -> exp) 
		in (annot, (Vdef (name,new_expr)))
		else (annot, Vdef (name,exp)))
	| other -> other

let rec type_paired_program = function
	| (x::xs) -> let (annot, (Vdef (name,exp))) = catch_main x in
		let (texpr, infty) = infer_type exp in
		let tpair = (annot, (TypedVdef (name, (texpr,infty) ) ) ) in
		tpair :: (type_paired_program xs)
	| [] -> []
