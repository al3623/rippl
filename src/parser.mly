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
%token LRANGE WILDCARD RARROW TLIT FLIT PLUS MINUS DIVIDE TIMES POW MOD 
%token PLUSF MINUSF
%token DIVIDEF TIMESF POWF OR AND NOT EQ EQF NEQ NEQF LESS LESSF GREATER 
%token GREATERF LEQ LEQF GEQ GEQF LEN CONS HEAD CAT TAIL ASSIGN BAR NEWLINE
%token DOUBLECOL INTTYPE FLOATTYPE BOOLTYPE CHARTYPE
%token INT_TO_FLOAT
%token MAYBE JUST NONE APP
%token FIRST SEC
%token IS_NONE FROM_JUST

%token <char> CHARLIT
%token <int> INTLIT
%token <string> STRLIT
%token <float> FLOATLIT
%token <string> IDENT

%left IN
%left OR AND NOT
%left EQ EQF NEQ NEQF LESS LESSF GREATER GREATERF LEQ LEQF GEQF GEQ
%right RARROW
%nonassoc MAYBE
%left ELSE

%left ASSIGN
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIVIDE MOD TIMESF DIVIDEF
%nonassoc UMINUS UMINUSF
%left POW POWF
%left CONS CAT
%nonassoc FIRST SEC LEN TAIL HEAD
%nonassoc JUST IS_NONE FROM_JUST
%left APP
%left INT_TO_FLOAT /* TODO: CHECK THIS */
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
    | IDENT                         { Tvar($1) }
	| MAYBE ty						{ Tmaybe($2) }
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
	| expr MOD expr			{ App (App(Mod, $1), $3) }

	| LPAREN OR RPAREN          { Or }
    | LPAREN AND RPAREN         { And }
	| LPAREN EQ RPAREN          { Eq }
        | LPAREN NOT RPAREN     { Not }
    | LPAREN EQF RPAREN         { EqF }
    | LPAREN NEQ RPAREN         { Neq }
    | LPAREN NEQF RPAREN        { NeqF }
    | LPAREN LESS RPAREN        { Less }
    | LPAREN LESSF RPAREN       { LessF }
    | LPAREN GREATER RPAREN     { Greater }
    | LPAREN GREATERF RPAREN    { GreaterF }
    | LPAREN LEQ RPAREN         { Leq }
    | LPAREN LEQF RPAREN        { LeqF }
    | LPAREN GEQ RPAREN         { Geq }
    | LPAREN GEQF RPAREN        { GeqF }
	| LPAREN MOD RPAREN			{ Mod }

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
    | MINUSF expr %prec UMINUSF { App(NegF, $2) }

	| LPAREN PLUS RPAREN 		{ Add }
    | LPAREN MINUS RPAREN       { Sub }
    | LPAREN TIMES RPAREN       { Mult }
    | LPAREN DIVIDE RPAREN      { Div }
    | LPAREN PLUSF RPAREN       { AddF }
    | LPAREN MINUSF RPAREN      { SubF }
    | LPAREN TIMESF RPAREN      { MultF }
    | LPAREN DIVIDEF RPAREN     { DivF }
    | LPAREN POW RPAREN         { Pow }
    | LPAREN POWF RPAREN        { PowF }

    /* LIST OPERATIONS */
    | expr CONS expr        { App (App(Cons, $1), $3) }
    | HEAD expr             { App(Head, $2) }
    | TAIL expr             { App (Tail, $2) }
    | expr CAT expr         { App (App(Cat, $1), $3)}
    | LEN expr              { App (Len, $2)}

	| LPAREN CONS RPAREN        { Cons }
	| LPAREN CAT RPAREN         { Cat }
        | LPAREN HEAD RPAREN    { Head }
        | LPAREN TAIL RPAREN    { Tail }
        | LPAREN LEN RPAREN     { Len }

    /* LITERALS */
    | literals              { $1 }

    /* PARENTHESIZED EXPRESSIONS */
    | LPAREN expr RPAREN %prec PAREN {$2}
	
	/* TUPLES */
	| LPAREN expr COMMA expr                        RPAREN { Tuple($2,$4) }
	| FIRST expr					{ App(First, $2) }
	| SEC expr						{ App(Sec, $2) }

	/* MAYBE */
	| JUST expr						{ Just($2) }
	| NONE							{ None }
	| IS_NONE expr					{ App(Is_none, $2) }
	| FROM_JUST expr				{ App(From_just, $2) }

	| INT_TO_FLOAT expr				{ App(Int_to_float, $2) }
	
	/* LIST OPERATORS */
    
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
	| LBRACK list_comp RBRACK { $2 }

prim_list:
    | 						{ [] }
    | expr                  { [$1] }
    | expr COMMA prim_list  { $1 :: $3 }

list_range:
	| expr LRANGE expr 		{ ListRange($1,$3) }

list_comp:
	| expr BAR clauses		{ ListComp($1,$3) }

clauses:
	| clause				{ [$1] }
	| clause COMMA clauses	{ $1 :: $3 }

clause:
    | expr { Filter($1) } /*boolean filter for list comp*/
    | IDENT OVER lists { ListVBind($1,$3) } /*variable binding for list comp*/
	| IDENT OVER IDENT { ListVBind($1,Var($3)) }
