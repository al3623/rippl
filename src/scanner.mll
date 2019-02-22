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
            | digit+ as lit 	{ INTLIT(int_of_string lit) }
            (* BOOLEAN LITERALS *)
            | "true"            { TLIT }
            | "false"           { FLIT }
            (* CHAR LITERALS *)
            | "\'"              { char_literal lexbuf }
            (* STRING LITERALS *)
            | "\""              { string_literal lexbuf }
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
    and comment [nestCount] = 
        parse "-}"              { if nestCount = 0 then token lexbuf else
                                    comment (nestCount - 1) lexbuf}
            | "{-"              { comment (nestCount + 1) lexbuf }
            | _                 { comment lexbuf }
    and string_literal =
        parse
    and char_literal =
            parse "\\\\"        { CHARLIT('\\') } (* OHGODOHFUCK *)
            | "\\n"             { CHARLIT('\n') }
            | "\\t"             { CHARLIT('\t') }
            | "\\r"             { CHARLIT('\r') }
            | _ as c             { CHARLIT(c) }
    and end_char_literal =
        parse "/'"              { token lexbuf }


