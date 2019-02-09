{ open Parser }

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
            | ['0'-'9']+ as lit { INTLIT(int_of_string lit) }
            (* BOOLEAN LITERALS *)
            | "true"            { TLIT }
            | "false"           { FLIT }
            (* CHAR LITERALS *)
            (* STRING LITERALS *)
            (* NUM OPERATORS *)
            | '+'               { PLUS }
            | '-'               { MINUS }
            | '/'               { DIVIDE }
            | '*'               { TIMES }
            | '^'               { POW }
            | '%'               { MOD }
            | "+."              { FPLUS }
            | "-."              { MINUSF }
            | "/."              { DIVF }
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
            | '<'               { LESSER }
            | "<."              { LESSERF }
            | '>'               { GREATER }
            | ">."              { GREATERF }
            | "<="              { LEQ }
            | "<=."             { LEQF }
            | ">=."             { GEQF }
            | ">="              { GEQ }
            (* LIST OPERATORS *)
            | "pre"             { PRE }
            | "head"            { HEAD }
            | "tail"            { TAIL }
            | "++"              { CONCAT }
            (* ASSIGN *)
            | '='               { ASSIGN }
            (* WHITESPACE *)
            | ' '               { token lexbuf }
            | '\t'              { TAB }
            | '\n'              { NEWLINE }
            (* COMMENTS *)
            | "::"              { TYPECOL }
            | "--"              { COMMENT }
