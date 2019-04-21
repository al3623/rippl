%{ open Ast
   open String
   open List

   let to_char_lit c = Ast.CharLit c
   let explode s =
        let rec exp i l =
            if i < 0 then l else exp (i - 1) (s.[i] :: l) in
        exp (String.length s - 1) []
   let char_lit_list str = List.map to_char_lit (explode str)
%}

%token EOF LET IN IF THEN ELSE OVER FUN LBRACK RBRACK LPAREN RPAREN COMMA
%token LRANGE WILDCARD RARROW TLIT FLIT PLUS MINUS DIVIDE TIMES POW MOD PLUSF MINUSF
%token DIVIDEF TIMESF POWF OR AND NOT EQ EQF NEQ NEQF LESS LESSF GREATER 
%token GREATERF LEQ LEQF GEQ GEQF LEN CONS HEAD CAT TAIL ASSIGN BAR NEWLINE
%token DOUBLECOL INTTYPE FLOATTYPE BOOLTYPE CHARTYPE
%token MAYBE JUST NONE APP


%token <char> CHARLIT
%token <int> INTLIT
%token <string> STRLIT
%token <float> FLOATLIT
%token <string> IDENT

%left IN
%left RARROW
%left APP
%left MAYBE
%left ELSE

%left ASSIGN
%left OR AND NOT EQ EQF NEQ NEQF LESS LESSF GREATER GREATERF LEQ LEQF GEQF GEQ
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIVIDE MOD TIMESF DIVIDEF
%nonassoc UMINUS
%left POW POWF
%left CONS HEAD TAIL CAT LEN
%nonassoc PAREN

%start program
%type <Ast.program> program

%%

program:
    | decl EOF                       { [$1] }
    | decl program             { $1 :: $2 }

decl:
    | vdef                          { $1 }
    | annotation                    { $1 }

vdef:
    | IDENT ASSIGN expr                 { Vdef($1,$3)}

ty:
    | BOOLTYPE                      { Bool }
    | INTTYPE                       { Int }
    | CHARTYPE                      { Char }
    | FLOATTYPE                     { Float }
    | LBRACK ty RBRACK              { TconList($2) }
    | LPAREN ty COMMA ty RPAREN     { TconTuple($2,$4) }
    | MAYBE ty                      { Tmaybe($2) }
    | IDENT                         { Tvar($1) }
    | ty RARROW ty                  { Tarrow($1,$3) }

annotation:
    | IDENT DOUBLECOL ty            { Annot($1,$3) }

assign:
    | IDENT ASSIGN expr              { Assign($1, $3) }

expr:
        
    /* SYNTACTIC EXPRESSIONS */
    
    | IF expr THEN expr ELSE expr { Ite($2,$4,$6) }
    | LET assign IN expr            { Let($2,$4) }
    | FUN IDENT RARROW expr        { Lambda($2,$4) }
    /* | expr expr                { App($1,$2) } */
    | expr APP expr                   { App($1,$3) }
    | IDENT                       { Var($1) }
    | WILDCARD                    { WildCard }
    
    | lists                         { $1 }

    /* BOOLEAN OPERATIONS */
    | expr OR expr          { App (App(Or, $1), $3) }
    | expr AND expr         { App (App(And, $1), $3) }
    | NOT expr              { App(Not, $2) }
    | expr EQ expr          { App (App(Eq, $1), $3) }
    | expr EQF expr         { App (App(EqF, $1), $3) }
    | expr NEQ expr         { App (App(Neq, $1), $3) }
    | expr NEQF expr        { App (App(NeqF, $1), $3) }
    | expr LESS expr        { App (App(Less, $1), $3) }
    | expr LESSF expr       { App (App(LessF, $1), $3) }
    | expr GREATER expr     { App (App(Greater, $1), $3) }
    | expr GREATERF expr    { App (App(GreaterF, $1), $3) }
    | expr LEQ expr         { App (App(Leq, $1), $3) }
    | expr LEQF expr        { App (App(LeqF, $1), $3) }
    | expr GEQ expr         { App (App(Geq, $1), $3) }
    | expr GEQF expr        { App (App(GeqF, $1), $3) }

    /* MATH OPERATIONS */
    | expr PLUS expr        { App (App(Add, $1), $3) }
    | expr MINUS expr       { App (App (Sub, $1), $3) }
    | expr TIMES expr       { App (App (Mult, $1), $3) }
    | expr DIVIDE expr      { App (App(Div, $1), $3) }
    | expr PLUSF expr       { App (App(AddF, $1), $3) }
    | expr MINUSF expr      { App (App(SubF, $1), $3) }
    | expr TIMESF expr      { App (App(MultF, $1), $3) }
    | expr DIVIDEF expr     { App (App(DivF, $1), $3) }
    | expr POW expr         { App (App(Pow, $1), $3) }
    | expr POWF expr        { App (App(PowF, $1), $3) }
    | MINUS expr %prec UMINUS { App(Neg, $2) }

    /* LIST OPERATIONS */
    | expr CONS expr        { App (App(Cons, $1), $3) }
    | HEAD expr             { App(Head, $2) }
    | TAIL expr             { App (Tail, $2) }
    | expr CAT expr         { App (App(Cat, $1), $3)}
    | LEN expr              { App (Len, $2)}

    /* LITERALS */
    | literals              { $1 }

    /* PARENTHESIZED EXPRESSIONS */
    | LPAREN expr RPAREN %prec PAREN {$2}
    
literals:
    /* PRIMITIVE LITERALS */
    | FLIT                  { BoolLit(false) }
    | TLIT                  { BoolLit(true) }
    | CHARLIT               { CharLit($1) } 
    | STRLIT                { ListLit((char_lit_list $1)) }
    | INTLIT                { IntLit($1) }
    | FLOATLIT              { FloatLit($1) }

lists:
	| LBRACK prim_list RBRACK { ListLit($2) }
	| LBRACK list_range RBRACK { $2 }
	| LBRACK inf_list RBRACK { $2 }
	| LBRACK list_comp RBRACK { $2 }

prim_list:
    | 						{ [] }
    | expr                  { [$1] }
    | expr COMMA prim_list  { $1 :: $3 }

list_range:
	| expr LRANGE expr 		{ ListRange($1,$3) }

inf_list:
	| expr LRANGE			{ InfList($1) }

list_comp:
	| expr BAR clauses		{ ListComp($1,$3) }

clauses:
	| clause				{ [$1] }
	| clause COMMA clauses	{ $1 :: $3 }

clause:
    | expr { Filter($1) } /*boolean filter for list comp*/
    | IDENT OVER lists { ListVBind($1,$3) } /*variable binding for list comp*/
