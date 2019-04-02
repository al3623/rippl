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

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
		let pair_program = Pair_annots.pair_av program in
		let pair_tprogram = Type_inference.type_paired_program pair_program in
        let m = Codegen.translate pair_tprogram in
        Llvm_analysis.assert_valid_module m;
        let ls = Llvm.string_of_llmodule m in
        let file = "hello.rply" in
        let oc = open_out file in
		fprintf oc "%s\n" ls;
        close_out oc;
        Sys.command ("cat " ^ file ^ " | lli")
