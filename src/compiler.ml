open Ast
open Tast
open Scanner
open Parser
open Check_main
open Pair_annots
open Pretty_type_print
open Lift_lambdas
open Check_lists
open Printf
open Remove_substs
open Sys
open String
open Thunk
open Unix
module StringSet = Set.Make(String)

let print_decls d = match d with
        | Vdef(n, e) -> print_endline (n ^ " = " 
			^ (Pretty_type_print.ast_to_str (e)) ^ "\n");
        | Annot(s, t) -> print_endline (s ^ " :: " 
			^ (Pretty_type_print.ty_to_str (t)) ^ "\n")
 
        
let rec remove_path str =
	let slash = index_opt str '/' in
	match slash with
	| (Some i) -> remove_path (sub str (i+1) ((length str) -i-1))
	| None -> str

let print_tdecl td = 
    match td with
    | (annot, tvd) -> print_endline ( Pretty_tast_print.tast_to_str tvd)

let rec print_all_types = function
	| (_, TypedVdef(name, (_,ty)))::xs -> 
		print_endline (name ^ ": " ^ (ty_to_str ty));
		print_all_types xs
	| [] -> print_newline ()

let read_full_file fname =
			let ch = open_in fname in
			let s = really_input_string ch (in_channel_length ch) in
			close_in ch;
			s

let collect_args =
	let num_args = Array.length argv in
	let rec collect_args_n = function
		| 0 -> []
		| n -> 	let argn = argv.(n) in
				let len = length argn in
				if len > 2 then
				if (sub argn (len-3) 3) = "rpl"
				then argn::(collect_args_n (n-1))	
				else (collect_args_n (n-1)) @ [argn]
				else (collect_args_n (n-1)) @ [argn]
	in collect_args_n (num_args-1)

let rec parse_flags = function
	| (flag::xs) -> let (t,l,p) = parse_flags xs in
		if flag = "-t" 
		then (true,l,p)
		else if flag = "-l" 
		then (t,true,p)
		else if flag = "-p"
		then (t,l,true)
        else (eprintf "Usage: <.rpl file> [-t, -l, -p]"; ignore (exit 0); 
        (false,false,false))
	| [] -> (false, false, false)

let _ =
	let all_args = collect_args in
	let input = List.hd all_args in
	let length = length input in
	(
	if length > 4 
		then ()
        else (eprintf "Usage: <.rpl file> [-t, -l, -p]"; ignore (exit 0); ())
	);
	let base = sub input 0 (length-4) in
	let base_no_path = remove_path base in
	let extension = sub input (length-3) 3 in

	let (printTypes,printLifted,printDecl) = parse_flags (List.tl all_args )in

	let _ = if extension = "rpl" 
			then ()
            else (eprintf "Usage: <.rpl file> [-t, -l, -p]"; ignore(exit 0); ()) 
    in

	let file_contents = read_full_file input in
    
	let lexbuf = Lexing.from_string file_contents in
    let program = Parser.program Scanner.token lexbuf in
	(if printDecl && (not printLifted)
		then List.iter print_decls program
		else ());
    let m_program = Lift_lambdas.transform_main program false in
    let program_ll = Lift_lambdas.close_and_lift m_program in
	(if printLifted
		then List.iter print_decls program_ll
		else ());
	let pair_program = Pair_annots.pair_av program_ll in
    let (subst,pair_iprogram) = Type_inference.type_paired_program pair_program in
	let pair_tprogram = Remove_substs.remove_subst_pairs subst pair_iprogram in
	(if printTypes
		then print_all_types pair_tprogram
		else ());
    let m = Codegen.translate pair_tprogram in
	    Llvm_analysis.assert_valid_module m;
    let ls = Llvm.string_of_llmodule m in
    let file = base_no_path ^ ".byte" in
	let home = Unix.getenv "HOME" in
        if (not printTypes && not printLifted) then (let oc = open_out file 
        in
		fprintf oc "%s\n" ls;
                close_out oc;
		if (command ("llc -relocation-model=pic " ^ file) != 0)
			then raise (Failure "llc: non-zero exit code") 
		else if (command 
			("gcc -L"^home^"/rippl/src "
			^ file ^".s -lall -lm  -o"
			^ base_no_path ) != 0)
			then raise (Failure "gcc: non-zero exit code")
		else ()) else ()
