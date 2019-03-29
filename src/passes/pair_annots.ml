open Ast
open Scanner
open Parser

let counter = ref 0
let rec pair_helper decs last_annot annot = 
	if (List.length decs) = 0 then 
		if not last_annot then [] else raise (Failure "unmatched type annotation")
	else match (List.hd decs) with
	| Annot(name, tname) -> (match last_annot with
		| true -> raise (Failure "unmatched type annotation")
		| false -> pair_helper (List.tl decs) true (name, tname))
	| Vdef(vname, vexpr) -> (match last_annot with
		| true -> 
			if String.equal vname (fst annot) then 
				(Annot(fst annot, snd annot), Vdef(vname, vexpr)) :: pair_helper (List.tl decs) false annot
			else raise (Failure "mismatched identifier name in annotation and declaration")
		| false -> 
			let vapair = (Annot(vname, Tvar("t" ^ string_of_int !counter)), Vdef(vname, vexpr)) in
			counter := !counter + 1;
			vapair :: pair_helper (List.tl decs) false annot)
let pair_av prog = match prog with
	| x :: xs -> pair_helper (x :: xs) false ("", Tvar(""))
	| [] -> []
