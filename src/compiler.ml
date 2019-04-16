open Ast
open Scanner
open Parser
open Check_main
open Pair_annots
open Pretty_type_print
open Type_inference
open Check_lists
open Codegen
open Printf
open Sys
open String
open Thunk

let rec remove_path str =
	let slash = index_opt str '/' in
	match slash with
	| (Some i) -> remove_path (sub str (i+1) ((length str) -i-1))
	| None -> str

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
	let pair_program = Pair_annots.pair_av program in
	let pair_tprogram = Type_inference.type_paired_program pair_program in
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
