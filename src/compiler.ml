open Ast
open Scanner
open Parser
open Check_main
open Pair_annots
open Pretty_type_print
(*open Type_check*)
open Check_lists
open Codegen

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let av_pair_list = pair_av program in
        let m = Codegen.translate program in
        Llvm_analysis.assert_valid_module m;
        print_string (Llvm.string_of_llmodule m)