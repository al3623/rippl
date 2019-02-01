{ open Parser }

rule token =
        parse eof               { EOF }
            (* KEYWORDS *) 
            | "let"             { LET }
            | "in"              { IN }
            | "if"              { IF }
            | "then"            { THEN }
            | "else"            { ELSE }
            (* BRACES *)
            | '['               { LBRACK }
            | ']'               { RBRACK }
            | '{'               { LBRACE }
            | '}'               { RBRACE }
            | '('               { LPAREN }
            | ')'               { RPAREN }
            (* MISC *)
            | ','               { COMMA }
            | ".."              { LRANGE }
            | '\\'              { LAMDAB }
            | "->"              { RARROW }
            | "<-"              { LARROW }
            (* NUM LITERALS *)
            | ['0'-'9']+ as lit { INTLIT(int_of_string lit) }
            (* BOOLEAN LITERALS *)
            | "True"            { TLIT }
            | "False"           { FLIT }
            (* CHAR LITERALS *)
            (* STRING LITERALS *)
            (* NUM OPERATORS *)
            | '+'               { PLUS }
            | '-'               { MINUS }
            | '/'               { DIVIDE }
            | '*'               { TIMES }
            | '%'               { MOD }
            | "+f"              { FPLUS }
            | "-f"              { MINUSF }
            | "/f"              { DIVF }
            | "*f"              { TIMESF }
            (* BOOLEAN OPERATORS *)
            | "||"              { OR }
            | "&&"              { AND }
            | "=="              { EQ }
            | "!="              { NEQ }
            | '<'               { LESSER }
            | '>'               { GREATER }
            | "<="              { LEQ }
            | ">="              { GEQ }
            (* LIST OPERATORS *)
            | ':'               { CONS }
            | "++"              { CONCAT }
            (* ASSIGN *)
            | '='               { ASSIGN }
            (* WHITESPACE *)
            | ' '               { token lexbuf }
            | '\t'              { TAB }
            | '\n'              { NEWLINE }
            (* COMMENTS *)
            | "--"              { COMMENT }
