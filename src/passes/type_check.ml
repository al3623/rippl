open Ast
open Scanner
open Parser
open Type_inference

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
    | Tforall ([var1], (Tvar _)) -> (function | _ -> true)
    | Tforall ([], ty) -> (function | ty' -> true)
    (* We shouldn't see Tvars in a top-level annot? *)

(* Assuming identifiers are the same *)
let check_annot = function
    | (Annot (_, annot_ty), Vdef (_, expr)) ->
        let inferred_ty = infer_type expr in
        subtype inferred_ty annot_ty

let rec type_check = function
    | x :: xs -> (check_annot x) && (type_check xs)
    | [] -> true
