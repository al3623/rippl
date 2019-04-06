open Ast
open Scanner
open Parser
module StringSet = Set.Make(String)


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
	| Let(Assign(nl, Lambda(_,_)), e1) -> StringSet.union (find_lambdas nested exp) (get_closure_vars e1 (StringSet.add nl scope) nested)
	| Let(Assign(na, e6), e1) -> let new_scope = (StringSet.add na scope) in StringSet.union (get_closure_vars e6 new_scope nested) (get_closure_vars e1 new_scope nested)
	| Var(s1) -> if StringSet.mem s1 scope then StringSet.empty else (StringSet.add s1 StringSet.empty)
	| Lambda(e2, e3) -> find_lambdas nested exp
	| App(e4, e5) -> StringSet.union (get_closure_vars e4 scope nested) (get_closure_vars e5 scope nested)
	| _ -> StringSet.empty

and find_lambdas nested = function
	| Let(Assign(ln, Lambda(Var(p), e8)), e9) ->
		let vs1 = get_closure_vars e8 (StringSet.add p StringSet.empty) true  in
		print_endline ("lambda name:" ^ ln ^ ";param: " ^ p ^ ";closed variables[ ");
		StringSet.iter (print_endline) vs1;
		print_endline "]";
		if not nested then let _ = find_lambdas nested e9 in vs1 else vs1
    | Let(Assign(_, e10) ,e1) -> let _ = find_lambdas nested e10 in let _ = find_lambdas nested e1 in StringSet.empty
    | App(e2, e3) -> let _ = find_lambdas nested e2 in let _ =  find_lambdas nested e3 in StringSet.empty
    | Ite(e4, e5, e6) -> let _ = find_lambdas nested e4 in let _ =  find_lambdas nested e5 in let _ = find_lambdas nested e6 in StringSet.empty
    | Lambda(Var(p2), e10) -> print_endline ("lambda name: anon" ^ "; param: " ^ p2 ^"; inner_vars[ ");
    	let vs3 = get_closure_vars e10 (StringSet.add p2 StringSet.empty) nested in
    	StringSet.iter (print_endline) vs3;
		print_endline "]"; vs3
    | _ -> print_endline ""; StringSet.empty





(*main _ =
	let a = 5 in
	let f1 = (fun x -> 
		let b = 3 in
		(b + 10)) in
	f1 . a*)