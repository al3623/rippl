open Ast
open Scanner
open Parser
open Get_fresh_var
open Printf
module StringSet = Set.Make(String)

let lamb_to_cl = Hashtbl.create 42069

let rec transform_main d_list = match d_list with 
	| Vdef (name, exp) :: ds1 -> (if name = "main" 
		then let new_expr = (match exp with
		| Lambda (_,ret_expr) -> ret_expr
		| _ -> exp) 
		in Vdef (name,new_expr)
		else Vdef (name,exp)) :: transform_main ds1
	| other :: ds2 -> other :: transform_main ds2
	| [] -> []

let rec get_closure_vars exp scope nested= match exp with
	| Let(Assign(nl, Lambda(_,_)), e1) -> 
		let (il_scope, il_structure) = find_lambdas nested exp in
		let (rest_scope, rest_structure) = get_closure_vars e1 (StringSet.add nl scope) nested in

		let complete_il_structure = 
		(match il_structure with 
		| Let(Assign(nl2 ,Lambda(Var(p), def_expr)), WildCard) -> Let(Assign(nl2, Lambda(Var(p), def_expr)), rest_structure)
		| _ -> WildCard) in

		((StringSet.union il_scope rest_scope), complete_il_structure)

	| Let(Assign(na, e6), e1) -> 
		let new_scope = (StringSet.add na scope) in 
		let (sc1, st1) = (get_closure_vars e6 new_scope nested) in
		let (sc2, st2) = (get_closure_vars e1 new_scope nested) in
		let new_expr = Let(Assign(na, st1), st2) in

		((StringSet.union sc1 sc2), new_expr)


	| Var(s1) -> 
		let new_scope = if StringSet.mem s1 scope then StringSet.empty else (StringSet.add s1 StringSet.empty) in

		(new_scope, Var(s1))

	| App(e4, e5) ->
		let (sc1, st1) = (get_closure_vars e4 scope nested) in
		let (sc2, st2) = (get_closure_vars e5 scope nested) in

		(StringSet.union sc1 sc2, App(st1, st2))

	| Lambda(e2, e3) -> 
		find_lambdas nested exp

	| other -> (StringSet.empty, other)

and find_lambdas nested = function
	| Let(Assign(ln, Lambda(Var(p), e8)), e9) ->
		let (lsc, lst) = get_closure_vars e8 (StringSet.add p StringSet.empty) true  in
		(*print_endline ("lambda name:" ^ ln ^ ";param: " ^ p ^ ";closed variables[ ");
		StringSet.iter (print_endline) vs1;
		print_endline "]";*)
		let (_, rest_st) = if not nested then find_lambdas nested e9
								else (lsc, WildCard (* let helper construct this *) ) in

		let new_expr = Let(Assign(ln ,Lambda(Var(p), lst)), rest_st) in
		let mangled_name = get_fresh ("$" ^ ln) in
		Hashtbl.add lamb_to_cl ln (mangled_name, lsc);

		(lsc, new_expr)

    | Let(Assign(n, e10) ,e1) ->

    	let (_, st10) = find_lambdas nested e10 in 
    	let (_, st1) = find_lambdas nested e1 in 

    	(StringSet.empty, Let(Assign(n, st10), st1))


    | App(e2, e3) -> 
    	let (_, st2) = find_lambdas true e2 in 
    	let (_, st3) =  find_lambdas true e3 in

    	(StringSet.empty, App(st2, st3))


    | Ite(e4, e5, e6) -> 
    	let (_, st4) = find_lambdas nested e4 in 
    	let (_, st5) =  find_lambdas nested e5 in 
    	let (_, st6) = find_lambdas nested e6 in 

    	(StringSet.empty, Ite(st4, st5, st6))

    | Lambda(Var(p2), e10) -> 
    	if nested then 
			(*(print_endline ("lambda name: anon" ^ "; param: " ^ p2 ^"; closed variables[ ");*)
			let (sc10, st10) = get_closure_vars e10 (StringSet.add p2 StringSet.empty) nested in
			(*StringSet.iter (print_endline) vs3;
			print_endline "]"; vs3) else StringSet.empty*)

			let anon_name = get_fresh "$anon" in
			let new_expr = Let(Assign(anon_name, Lambda(Var(p2), st10)), Var(anon_name)) in

			Hashtbl.add lamb_to_cl anon_name (anon_name, sc10);

			(sc10, new_expr)
		else
			(StringSet.empty, Lambda(Var(p2), e10))

    | other -> (*print_endline "";*) (StringSet.empty, other)


let print_map _ =
	let print_closure c =
		Printf.printf " mangled name: %s; needed_params:" (fst c);
		Printf.printf " [ ";
		StringSet.iter (fun s1 -> Printf.printf "%s, " s1) (snd c);
		Printf.printf " ]\n" in
	Hashtbl.iter (fun x c -> Printf.printf "lambda: %s; " x; print_closure c;) lamb_to_cl



(*main _ =
	let a = 5 in
	let f1 = (fun x -> 
		let b = 3 in
		(b + 10)) in
	f1 . a*)