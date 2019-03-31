open Ast
open Scanner
open Parser
open Type_inference

let rec replace_tvar var new_ty = function
    | (Tvar tv) -> if tv = var then new_ty else (Tvar tv)
    | (Tarrow (t1,t2)) -> let t1' = replace_tvar var new_ty t1 in
        let t2' = replace_tvar var new_ty t2 in
        (Tarrow (t1',t2'))
    | (TconList ty) -> (TconList (replace_tvar var new_ty ty))
    | (TconTuple (t1,t2)) -> let t1' = replace_tvar var new_ty t1 in
        let t2' = replace_tvar var new_ty t2 in
        (TconTuple (t1',t2'))
    | (Tmaybe ty) -> (Tmaybe (replace_tvar var new_ty ty))
    (* Tforall ?  *)
    | prim_ty -> prim_ty

(*
let rec coincide var = function
    | (Tarrow (t1,t2))
    | (TconList t)
    | (TconTuple t)
    | (Tmaybe t) 
*)

(* Is the second type a subtype of the first *)
let rec subtype = function
    (* Primitives are their own only subtypes *)
    | Int   -> (function | Int -> true   | _ -> false)
    | Bool  -> (function | Bool -> true  | _ -> false)
    | Float -> (function | Float -> true | _ -> false)
    | Char  -> (function | Char -> true  | _ -> false)
    (* Lists *)
    | (TconList  ty) -> (function | (TconList ty') -> subtype ty ty'
                                  | _ -> false)
    (* Tuples *)
    | (TconTuple (ty1,ty2)) -> (function 
            | (TconTuple (ty3,ty4)) -> (subtype ty1 ty3) && (subtype ty2 ty4)
            | _ -> false)
    (* Tmaybe *)
    | Tmaybe ty -> (function | (Tmaybe ty') -> (subtype ty ty')
                             | _ -> false)
    (* Arrow types *)
    | (Tarrow (arg_ty, ret_ty)) -> (function
            | (Tarrow (arg_ty', ret_ty')) -> (subtype arg_ty arg_ty') &&
                (subtype ret_ty ret_ty')
            | _ -> false)
    (* Tforalls *)

    (* No Tvars in toplevel type *)

(* Assuming identifiers are the same *)
let check_annot = function
    | (Annot (_, annot_ty), Vdef (_, expr)) ->
        let inferred_ty = infer_type expr in
        subtype inferred_ty (simple_generalize annot_ty)

let rec type_check = function
    | x :: xs -> (check_annot x) && (type_check xs)
    | [] -> true
