open AST
open Scanner
open Parser

let _ =
		let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let rec check_lists prog =
        match prog with 
            | x :: xs -> ( match x with
                    | Vdef(ident, expr) -> (
                        match expr with (* I'm not exactly sure if 
                        this is the right way to find the list comprehension
                        within the program... Do I have to check every
                        possible construct that could contain an expr?*)
                            | ListComp(expr, clist) -> (
                                match expr with 
                                | App(App(Add, _), _) -> "GOOD COMP"
                                | App(App(Sub, _), _) -> "GOOD COMP"
                                | App(App(Mult, _), _) -> "GOOD COMP"
                                | App(App(Div, _), _) -> "GOOD COMP"       
                                | App(App(Mod, _), _) -> "GOOD COMP"
                                | App(App(Pow, _), _) -> "GOOD COMP"
                                | App(App(AddF, _), _) -> "GOOD COMP"
                                | App(App(SubF, _), _) -> "GOOD COMP" 
                                | App(App(MultF, _), _) -> "GOOD COMP" 
                                | App(App(DivF, _), _) -> "GOOD COMP" 
                                | App(App(PowF, _), _) -> "GOOD COMP" 
                                | App(App(Neg, _), _) -> "GOOD COMP" 
                                | _ -> "BAD COMP"
                            )
                            | _ -> check_lists xs
                        )
                    | Annot(_,_) -> check_lists xs
            )
            | []-> ""
        in check_lists program