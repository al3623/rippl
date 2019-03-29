open Ast
open Scanner
open Parser

(* Is the first type a subtype of the second *)
let rec subtype = function
    | Int -> 
    | Bool ->
    | Float ->
    | Char ->

(* Assuming identifiers are the same *)
let check_annot = function
    | (Annot (_, ty), Vdef (_, expr)) ->
        let inferred_ty = infer_type expr in
        subtype ty inferred_ty

let rec type_check = function
    | x :: xs -> (check_annot x) && (type_check xs)
    | [] -> true
