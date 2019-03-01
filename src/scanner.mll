{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token =
        parse eof               { EOF }
            (* KEYWORDS *) 
            | "let"             { LET }
            | "in"              { IN }
            | "if"              { IF }
            | "then"            { THEN }
            | "else"            { ELSE }
            | "over"            { OVER }
            | "fun"             { FUN }
            (* BRACES *)
            | '['               { LBRACK }
            | ']'               { RBRACK }
            | '('               { LPAREN }
            | ')'               { RPAREN }
            (* MISC *)
            | ','               { COMMA }
            | "..."             { LRANGE }
            | "->"              { RARROW }
            | '|'               { BAR }
            (* NUM LITERALS *)
            | ('-' ?)digit+ as lit 	{ INTLIT(int_of_string lit) }
            | ('-' ?)((digit+ '.' digit+)) as lit { FLOATLIT(float_of_string lit)}
            (* BOOLEAN LITERALS *)
            | "true"            { TLIT }
            | "false"           { FLIT }
            (* CHAR LITERALS *)
            | "'" ( _ as c) "'" { CHARLIT(c) }
            | '\'' '\\' 'n' '\''     { CHARLIT('\n') }
            (* STRING LITERALS *)
            | '"' ([^ '"']* as str) '"'  { STRLIT(str) }
            (* NUM OPERATORS *)
            | '+'               { PLUS }
            | '-'               { MINUS }
            | '/'               { DIVIDE }
            | '*'               { TIMES }
            | '^'               { POW }
            | '%'               { MOD }
            | "+."              { PLUSF }
            | "-."              { MINUSF }
            | "/."              { DIVIDEF }
            | "*."              { TIMESF }
            | "^."              { POWF }
            (* BOOLEAN OPERATORS *)
            | "or"              { OR }
            | "and"             { AND }
            | "not"             { NOT }
            | "=="              { EQ }
            | "==."             { EQF}
            | "!="              { NEQ }
            | "!=."             { NEQF }
            | '<'               { LESS }
            | "<."              { LESSF }
            | '>'               { GREATER }
            | ">."              { GREATERF }
            | "<="              { LEQ }
            | "<=."             { LEQF }
            | ">=."             { GEQF }
            | ">="              { GEQ }
            (* LIST OPERATORS *)
            | "cons"            { CONS }
            | "head"            { HEAD }
            | "tail"            { TAIL }
            | "cat"             { CAT }
            (* ASSIGN *)
            | '='               { ASSIGN }
            (* IDENTIFIERS *)
            | (letter | '_') (letter | digit | '_')* as id { ID(id) }
            (* WHITESPACE *)
            | [' ' '\r' '\n' '\t']   { token lexbuf }
            | "{-"              { comment 0 lexbuf }
            | "#"              { line_comment lexbuf }
    and line_comment =
        parse '\n'              { token lexbuf }
            | _                 { line_comment lexbuf }
    and comment nestCount = 
        parse "-}"              { if nestCount = 0 then token lexbuf else
                                    comment (nestCount - 1) lexbuf}
            | "{-"              { comment (nestCount + 1) lexbuf }
            | _                 { comment nestCount lexbuf }
(* STRINGS AND CHAR LITERALS 
    and string_literal str =
        parse  
            | "\""              { STRLIT(str) ; token lexbuf }
    and char_literal =
            parse "\\\\"        { CHARLIT('\\') ; end_char_literal lexbuf } (* OHGODOHFUCK *)
            | "\\n"             { CHARLIT('\n') ; end_char_literal lexbuf }
            | "\\t"             { CHARLIT('\t') ; end_char_literal lexbuf }
            | "\\r"             { CHARLIT('\r') ; end_char_literal lexbuf }
            | _ as c            { CHARLIT(c) ; end_char_literal lexbuf }
    and end_char_literal =
        parse '\''              { token lexbuf }
*)

{

    let lexbuf = Lexing.from_channel stdin
    in
    let wordlist =
    let rec next l = 
                    match token lexbuf with
                    EOF -> l
            (* keywords *)
            | LET -> next ("LET" :: l)
            | IN -> next ("IN" :: l)
            | IF -> next ("IF" :: l)
            | THEN -> next ("THEN" :: l)
            | ELSE -> next ("ELSE" :: l)
            | OVER -> next ("OVER" :: l)
            | FUN -> next ("FUN" :: l)
            (* braces *)
            | LBRACK -> next ("LBRACK" :: l)
            | RBRACK -> next ("RBRACK" :: l)
            | LPAREN -> next ("LPAREN" :: l)
            | RPAREN -> next ("RPAREN" :: l)
            (* miscellaneous *)
            | COMMA -> next ("COMMA" :: l)
            | RARROW -> next ("RARROW" :: l)
            | LRANGE -> next("LRANGE" :: l)
            (* num literals *) 
            | INTLIT i -> next ("INT" :: l)
            | FLOATLIT f -> next ("FLOAT" :: l)
            (* boolean literals *)
            | TLIT -> next ("TLIT" :: l)
            | FLIT -> next ("FLIT" :: l)
            (* char and string literals*)
            | CHARLIT c -> next (("CHAR " ^ "<" ^ (String.make 1 c) ^ ">") :: l)
            | STRLIT str -> next (("STRING " ^ "<" ^ str ^ ">"):: l)
            (* numerical operators *)
            | PLUS -> next ("PLUS" :: l)
            | MINUS -> next ("MINUS" :: l)
            | DIVIDE -> next ("DIVIDE" :: l)
            | TIMES -> next ("TIMES" :: l)
            | POW -> next ("POW"::l)
            | MOD -> next ("MOD" :: l)
            | PLUSF -> next ("PLUSF" :: l)
            | MINUSF -> next ("MINUSF" :: l)
            | DIVIDEF -> next ("DIVIDEF" :: l)
            | TIMESF -> next ("TIMESF" :: l)
            | POWF -> next ("POWF" :: l)
            (* boolean operators *)
            | OR -> next ("OR" :: l)
            | AND -> next ("AND" :: l)
            | NOT -> next ("NOT" :: l)
            | EQ -> next ("EQ" :: l)
            | EQF -> next ("EQF" :: l)
            | NEQ -> next ("NEQ" :: l)
            | NEQF -> next ("NEQF" :: l)
            | LESS -> next ("LESS" :: l)
            | LESSF -> next ("LESSF" :: l)
            | GREATER -> next ("GREATER" :: l)
            | GREATERF -> next ("GREATERF" :: l)
            | LEQ -> next ("LEQ" :: l)
            | LEQF -> next ("LEQF" :: l)
            | GEQF -> next ("GEQF" :: l)
            | GEQ -> next ("GEQ" :: l)
            (* list operators *)
            | CONS -> next ("CONS" :: l)
            | HEAD -> next ("HEAD" :: l)
            | TAIL -> next ("TAIL" :: l)
            | CAT -> next ("CAT" :: l)
            | BAR -> next ("BAR" :: l)
            (* assignment *)
            | ASSIGN -> next ("ASSIGN" :: l)
            (* identifiers *)
            | ID id -> next ("IDENT" :: l)

            | NEWLINE -> next("\n"::l) 
            
            (* | _ -> next ("TOKEN" :: l) *)
        in
        next []
    in
    print_string "\n";
    let print_space str = print_string str; print_string " " in 
List.iter print_space (List.rev wordlist)



}

