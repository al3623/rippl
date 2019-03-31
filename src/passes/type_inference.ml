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
    | IntLit i -> ((IntLit i), Int)
    | FloatLit f -> ((FloatLit f),Float)
    | CharLit c -> ((CharLit c),Char)
    | BoolLit b -> ((BoolLit b),Bool)
    | ListLit l -> ( match l with
        (* TODO: check type of the entire list later *)
        | x::xs -> let (_,ty) = infer_type x in
            (List.map infer_type (x::xs), TconList ty)
        | [] ->  ([], (Tvar "a")))
(*    | Assign (e1,e2) -> ( match e1 with
        | (Var v) -> let itype = infer_type e2
            in Assign ()
        (* Can only assign to a variable *)
        | _ -> raise (Failure "not an lvalue for assignemnt")        
    )*)
    | _ -> raise (Failure ":C")

