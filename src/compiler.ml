open Ast
open Scanner
open Parser
open Check_main
(*open Type_check*)
open Pair_annots
open Pretty_type_print

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let av_pair_list = pair_av program in
        print_annot_pairs av_pair_list