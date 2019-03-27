open Ast
open Scanner
open Parser
open Check_main

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let rec find_main prog =
        match prog with
            | x :: xs -> (
                match x with 
                    | Annot(_,_) -> find_main xs 
                    | Vdef(ident,_) -> if ident = "main" then "YAY" else
                            find_main xs
            )
            | [] -> "NAY"
       in print_endline (find_main program)
