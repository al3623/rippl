 open Ast

 let rec ty_to_str ty =
    match ty with
    | Bool -> "Bool"
    | Int -> "Int"
    | Char -> "Char"
    | Float -> "Float"
    | TconList(t) -> "TconList(" ^ (ty_to_str t) ^ ")"
    | TconTuple(t1,t2) -> "TconTuple(" ^ (ty_to_str t1) ^ "," ^ (ty_to_str t2) ^ ")"
    | Tmaybe(t) -> "Tmaybe(" ^ (ty_to_str t) ^ ")"
    | Tvar(t) -> t
    | Tarrow(t1,t2) -> "Tarrow(" ^ (ty_to_str t1) ^ "," ^ (ty_to_str t2) ^ ")"
    | Tforall(t) -> "Tforall"

let rec print_annot_pairs lst = match lst with
	| (Annot(n1, t), Vdef(n2, e)) :: tl ->
		print_endline ("a_name: " ^ n1 ^ ", v_name: " ^ n2 ^ ", type:" ^ (ty_to_str t));
		print_annot_pairs tl
	| [] -> print_endline "done"
	| _ -> print_endline "what" 