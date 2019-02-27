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
            (* NUM LITERALS *)
            | ('-' ?)digit+ as lit 	{ INTLIT(int_of_string lit) }
            | ('-' ?)((digit* '.' digit+)|(digit+ '.' digit*)) as lit     { FLOATLIT(float_of_string lit) }
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
            | LPAREN -> next ("LPAREN" :: l)
            | CHARLIT c -> next (("<" ^ (String.make 1 c) ^ ">") :: l)
            | INTLIT i -> next ("INT" :: l)
            | FLOATLIT f -> next ("FLOAT" :: l)
            | STRLIT str -> next (("<" ^ str ^ ">"):: l)
            | _ -> next ("TOKEN" :: l)

        in
        next []
    in
    print_string "\n" ; List.iter print_endline (List.rev wordlist)

}

