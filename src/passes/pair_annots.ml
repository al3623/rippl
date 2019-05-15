open Ast
open Scanner
open Parser
open Get_fresh_var

let rec pair_helper decs last_annot annot main_found = 
	if (List.length decs) = 0 then 
		if not last_annot then (if main_found then [] else raise(Failure "main not type annotated")) else raise (Failure "unmatched type annotation")
	else match (List.hd decs) with
	| Annot(name, tname) -> (match last_annot with
		| true -> raise (Failure "unmatched type annotation")
		| false -> pair_helper (List.tl decs) true (name, tname) (if not main_found then name = "main" else main_found))
	| Vdef(vname, vexpr) -> (match last_annot with
		| true -> 
			if vname = (fst annot) then 
				(Annot(fst annot, snd annot), Vdef(vname, vexpr)) :: (pair_helper (List.tl decs) false annot main_found)
			else raise (Failure "mismatched identifier name in annotation and declaration")
		| false -> 
			let vapair = (Annot(vname, Tvar(get_fresh "t")), Vdef(vname, vexpr)) in
			vapair :: (pair_helper (List.tl decs) false annot main_found))
let pair_av prog = match prog with
	| x :: xs -> pair_helper (x :: xs) false ("", Tvar("")) false
	| [] -> []
