open Ast
open Scanner
open Parser
open Get_fresh_var
open Printf
module StringSet = Set.Make(String)

let lamb_to_cl = Hashtbl.create 42069
let og_to_mang = Hashtbl.create 42069

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

		((StringSet.union (StringSet.diff il_scope scope) rest_scope), complete_il_structure)

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

	| InfList(e1) ->
    	let (sc1, st1) = (get_closure_vars e1 scope nested) in

    	(sc1, InfList(st1))


    | ListRange(e1, e2) ->
		let (sc1, st1) = (get_closure_vars e1 scope nested) in
		let (sc2, st2) = (get_closure_vars e2 scope nested) in

    	((StringSet.union sc1 sc2), ListRange(st1, st2))

    | ListComp(e1, e2) -> (match e2 with
    	| [] ->
    		let (sc1, st1) = (get_closure_vars e1 scope nested) in

    		(sc1, ListComp(st1, []))

    	| cl ->
    		let (sc1, st1) = (get_closure_vars e1 scope nested) in
    		let (trav_sc, trav_st) = List.fold_left (list_helperc_cl nested scope) (StringSet.empty, []) cl in

    		((StringSet.union sc1 trav_sc), ListComp(st1, List.rev trav_st)))


    | ListLit(e1) ->
    	if List.length e1 = 0 then (StringSet.empty, ListLit([])) 
    	else
    		let (trav_sc, trav_st) = List.fold_left (list_helperc_ex nested scope) (StringSet.empty, []) e1 in

    		(trav_sc , ListLit((List.rev trav_st))) 

	| Lambda(e2, e3) -> 
		let (il_scope, il_structure) = find_lambdas nested exp in
		((StringSet.diff il_scope scope), il_structure)

	| other -> (StringSet.empty, other)


and list_helperc_ex nested scope exl ex =
	let (sc, st) = get_closure_vars ex scope nested in

	((StringSet.union sc (fst exl), st :: (snd exl)))

and list_helperc_cl nested scope clal cl =
	let (sc, st) = get_closure_vars_clause cl scope nested in

	((StringSet.union sc (fst clal)), st :: (snd clal))

and get_closure_vars_clause exp scope nested = match exp with
	| Filter(e1) ->
    	let (sc1, st1) = get_closure_vars e1 scope nested in

    	(sc1, Filter(st1))

    | ListVBind(e1, e2) ->
    	let (sc1, st1) = find_lambdas true e1 in 
    	let (sc2, st2) =  find_lambdas true e2 in

    	((StringSet.union sc1 sc2), ListVBind(st1, st2))



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

    | InfList(e1) ->
    	let (_, st1) = find_lambdas nested e1 in

    	(StringSet.empty, InfList(st1))

	| ListRange(e1, e2) ->
		let (_, st1) = find_lambdas true e1 in 
    	let (_, st2) =  find_lambdas true e2 in

    	(StringSet.empty, ListRange(st1, st2))

    | ListLit(e1) ->
    	if List.length e1 = 0 then (StringSet.empty, ListLit([])) 
    	else
    		let trav_list = List.fold_left (list_helperf_ex nested) [] e1 in

    		(StringSet.empty, ListLit((List.rev trav_list))) 

    | ListComp(e1, e2) -> (match e2 with
	    | [] ->
	    	let (_, st1) = find_lambdas nested e1 in
	    	(StringSet.empty, ListComp(st1, []))
	    | _ ->
	    	let (_, st1) = find_lambdas nested e1 in
	    	let trav_list = List.fold_left (list_helperf_cla nested) [] e2 in
	    	(StringSet.empty, ListComp(st1, trav_list)))


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

and list_helperf_ex nested exl ex =
	let (_, st) = find_lambdas nested ex in
	st :: exl

and list_helperf_cla nested clal cla =
	let (_, st) = find_lambdas_clause nested cla in
	st :: clal

and find_lambdas_clause nested = function
	| Filter(e1) ->
    	let (_, st1) = find_lambdas nested e1 in

    	(StringSet.empty, Filter(st1))

    | ListVBind(e1, e2) ->
    	let (_, st1) = find_lambdas true e1 in 
    	let (_, st2) =  find_lambdas true e2 in

    	(StringSet.empty, ListVBind(st1, st2))


let print_map _ =
	let print_closure c =
		Printf.printf " mangled name: %s; needed_params:" (fst c);
		Printf.printf " [ ";
		StringSet.iter (fun s1 -> Printf.printf "%s, " s1) (snd c);
		Printf.printf " ]\n" in
	Hashtbl.iter (fun x c -> Printf.printf "lambda: %s; " x; print_closure c;) lamb_to_cl

let rec close_app la vars = match vars with
	| hd :: tl ->
		let app1 = App(la, Var(Hashtbl.find og_to_mang hd)) in
		close_app app1 tl
	| [] -> la




let rec m_replace og_ex m_ex ex = match ex with
	| App(e1, e2) -> App(m_replace og_ex m_ex e1, m_replace og_ex m_ex e2)
	| Ite(e1, e2, e3) -> Ite(m_replace og_ex m_ex e1, m_replace og_ex m_ex e2, m_replace og_ex m_ex e3)
	| Lambda(e1, e2) -> Lambda(e1, m_replace og_ex m_ex e2)
	| Let(Assign(s2, e2), e3) -> Let(Assign(s2, (m_replace og_ex m_ex e2)), (m_replace og_ex m_ex e3))
	| Var(s1) -> if ex = og_ex then m_ex else ex
	| other -> other




let rec close_lambda lam vars = match vars with
	| hd :: tl -> 
		let mang_param = get_fresh ("$" ^ hd) in
		let repl_lam = m_replace (Var(Hashtbl.find og_to_mang hd)) (Var(mang_param)) lam in
		Lambda(Var(mang_param), (close_lambda repl_lam tl))
	| [] -> lam





let rec mangle_lets e = match e with 
	| Var(s1) -> Var(s1)
	| App(e1, e2) -> App(mangle_lets e1, mangle_lets e2)
	| Ite(e3, e4, e5) -> Ite(mangle_lets e3, mangle_lets e4, mangle_lets e5)
	| Lambda(e6, e7) -> Lambda(e6, mangle_lets e7)
	| ListRange(e1, e2) -> ListRange(mangle_lets e1, mangle_lets e2)
	| InfList(e1) -> InfList(mangle_lets e1)
	| ListLit(elst) ->
		ListLit(List.rev (List.fold_left (fun l ex -> (mangle_lets ex) :: l) [] elst))
	| ListComp(e1, cl) ->
		let mangled_lst = List.rev(List.fold_left mangle_helperc [] cl) in
		ListComp(mangle_lets e1, mangled_lst)
	| Let(Assign(s2, e1), e6) ->
		let cl_res = Hashtbl.find_opt lamb_to_cl s2 in
		(match cl_res with
			| None -> 
				let man_n1 = get_fresh ("$" ^ s2) in
				Hashtbl.add og_to_mang s2 man_n1;
				let repl_expr = m_replace (Var(s2)) (Var(man_n1)) e6 in
				let mangl_expr = mangle_lets repl_expr in
				Let(Assign(man_n1, mangle_lets e1), mangl_expr)
			| Some(cp) -> 
				let man_n2 = fst cp in
				let cl_vars = StringSet.elements (snd cp) in
				let cl_lam = close_lambda e1 cl_vars in
				let cl_app = close_app (Var(man_n2)) cl_vars in
				let repl_expr2 = m_replace (Var(s2)) cl_app e6 in
				let mangl_expr2 = mangle_lets repl_expr2 in
				Let(Assign(man_n2, mangle_lets cl_lam), mangl_expr2)

		)
	| other -> other


and mangle_helperc clst clau= match clau with
	| Filter(e1) -> Filter(mangle_lets e1) :: clst
    | ListVBind(e1, e2) -> ListVBind(mangle_lets e1, mangle_lets e2) :: clst



let rec lift exp decl_list = match exp with
	| Let(Assign(ln, Lambda(Var(p), e8)), e9) ->
		let (new_lbody, new_dlist) = lift e8 decl_list in
		let lifted_l = Vdef(ln, Lambda(Var(p), new_lbody)) in
		lift e9 (new_dlist @ [lifted_l])
	| App(e1, e2) -> 
		let (body1, dlist1) = lift e1 decl_list in
		let (body2, dlist2) = lift e2 dlist1 in
		(App(body1, body2), dlist2)
	| Ite(e1, e2, e3) -> 
		let (body1, dlist1) = lift e1 decl_list in
		let (body2, dlist2) = lift e2 dlist1 in
		let (body3, dlist3) = lift e3 dlist2 in
		(Ite(body1, body2, body3), dlist3)
	| Let(Assign(n, e1), e2) -> 
		let (body1, dlist1) = lift e1 decl_list in
		let (body2, dlist2) = lift e2 dlist1 in
		(Let(Assign(n, body1), body2), dlist2)
	| Var(s1) -> 
		(Var(s1), decl_list)
	| Lambda(e1, e2) ->
		let (body2, dlist2) = lift e2 decl_list in
		(Lambda(e1, body2), dlist2)
	| other -> (other, decl_list)







