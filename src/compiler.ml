open Ast
open Tast
open Scanner
open Parser
open Check_main
open Pair_annots
open Pretty_type_print
(*open Type_check*)
open Lift_lambdas
open Check_lists
open Codegen
open Printf
open Type_inference
open Remove_substs
open Sys
open String
open Thunk

let print_decls d = match d with
        | Vdef(n, e) -> print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e);
                (*let (_, e_named_lambdas) = find_lambdas false e in ();*)
                (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e_named_lambdas);*)
                (*let _ =Lift_lambdas.print_map() in ()*)
        | _ -> print_endline "annots"

let lift_decl curr_list d = match d with
        | Vdef(n, e) ->
                let (_, nl_ast) = find_lambdas false e in ();
                let mang_ast = mangle_lets nl_ast in
                let (lifted, l_decs) = lift mang_ast [] in
                curr_list @ (l_decs @ [Vdef(n, lifted)])
        | annot -> curr_list @ [annot]
        
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
	| [] -> print_newline

let read_full_file fname =
			let ch = open_in fname in
			let s = really_input_string ch (in_channel_length ch) in
			close_in ch;
			s

let _ = 
	let input = argv.(1) in
	let length = length input in
	let base = sub input 0 (length-4) in
	let base_no_path = remove_path base in
	let extension = sub input (length-3) 3 in

	let _ = if extension = "rpl" 
			then ()
			else raise (Failure "Usage: <.rpl file>") in

	let file_contents = read_full_file input in
    
	let lexbuf = Lexing.from_string file_contents in
    let program = Parser.program Scanner.token lexbuf in
    let m_program = Lift_lambdas.transform_main program in
    let program_ll = List.fold_left lift_decl [] m_program in
	  let pair_program = Pair_annots.pair_av program_ll in
    let pair_iprogram = Type_inference.type_paired_program pair_program in
		let pair_tprogram = Remove_substs.remove_subst_pairs pair_iprogram in
		    print_all_types pair_tprogram;  
    let m = Codegen.translate pair_tprogram in
	    Llvm_analysis.assert_valid_module m;
    let ls = Llvm.string_of_llmodule m in
    let file = base_no_path ^ ".byte" in
    let oc = open_out file in
		fprintf oc "%s\n" ls;
        close_out oc;
		if (command ("llc -relocation-model=pic " ^ file) != 0)
			then raise (Failure "llc: non-zero exit code") 
		else if (command ("gcc -o " ^ base_no_path ^" " ^ file ^ ".s lib.o") != 0)
			then raise (Failure "gcc: non-zero exit code")
		else ()
