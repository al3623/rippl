open Ast
open Scanner
open Parser


let rec check_list_clauses clauses = 
    match clauses with
        | hd :: tl -> (
                match hd with 
                    | ListVBind(_, lst) -> "GOOD LIST COMPREHENSION"
                    | _ -> "BAD LIST COMPREHENSION"
                )
        | [] -> ""



let rec check_list_comps prog =
    match prog with 
        | x :: xs -> ( match x with
                | Vdef(ident, expr) -> (
                    match expr with
                        | ListComp(expr, lst) -> check_list_clauses lst
                        | _ -> check_list_comps xs
                    )
                | Annot(_,_) -> check_list_comps xs
        )
        | []-> "" 


