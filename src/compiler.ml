open Ast
open Scanner
open Parser
open Check_main

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        print_endline (find_main program)

