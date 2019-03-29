open Ast
open Scanner
open Parser
open Check_main
open Type_check
open Check_lists

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        print_endline (check_list_comps program)
