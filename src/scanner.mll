{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token =
        parse eof               { EOF }
            (* TYPES *)
            | "int"             { INTTYPE }
            | "char"            { CHARTYPE }
            | "float"           { FLOATTYPE }
            | "bool"            { BOOLTYPE }
            | "maybe"           { MAYBE }
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
            | "::"              { DOUBLECOL }
            | "->"              { RARROW }
            | '|'               { BAR }
            | '_'               { WILDCARD }
            (* NUM LITERALS *)
            | digit+ as lit 	{ INTLIT(int_of_string lit) }
            | ((digit+ '.' digit+)) as lit { FLOATLIT(float_of_string lit)}
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
            | "len"             { LEN }
			(* TUPLE OPERATORS *)
			| "first"			{ FIRST }
			| "sec"				{ SEC }
			(* MAYBE OPERATORS *)
			| "none"			{ NONE }
			| "just"			{ JUST }
			| "is_none"			{ IS_NONE }
			| "from_just"		{ FROM_JUST }
            (* ASSIGN *)
            | '='               { ASSIGN }
            (* APPLICATION *)
            | '~'               { APP }
			(* CONVERSION *)
			| "int_to_float"	{ INT_TO_FLOAT }
            (* IDENTIFIERS *)
            | (letter | '_') (letter | digit | '_')* as id { IDENT(id) }
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

