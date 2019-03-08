%{  type ty = Int | Bool | Float | Char
    | Tvar of string
    | Arrow of (ty * ty)
    | TconList of ty
    | TconTuple of ty
    | Tforall of ty

type expr =
    | IntLit of int | FloatLit of float
    | CharLit of char | StrLit of string
    | Add | Sub | Mult | Div | Mod | Pow
    | AddF | SubF | MultF | DivF | PowF
    | Eq | EqF | Neq | NeqF | Geq | GeqF | Leq | LeqF
    | And | Or 
    | Cons | Cat | Len | Head | Tail
    | Var of string
    | Let of (expr * expr)
    | Lambda of (expr * expr)
    | App of (expr * expr)
    | Ite of (expr * expr * expr) %}

%token EOF LET IN IF THEN ELSE OVER FUN MAIN LBRACK RBRACK LPAREN RPAREN COMMA
%token LRANGE RARROW TLIT FLIT PLUS MINUS DIVIDE TIMES POW MOD PLUSF MINUSF
%token DIVIDEF TIMESF POWF OR AND NOT EQ EQF NEQ NEQF LESS LESSF GREATER 
%token GREATERF LEQ LEQF GEQ GEQF LEN CONS HEAD CAT TAIL ASSIGN BAR NEWLINE
%token DOUBLECOL INTTYPE FLOATTYPE BOOLTYPE CHARTYPE
%token MAYBE JUST NONE


%token <char> CHARLIT
%token <int> INTLIT
%token <string> STRLIT
%token <float> FLOATLIT
%token <string> ID



%left ASSIGN
%left OR AND NOT EQ EQF NEQ NEQF LESS LESSF GREATER GREATERF LEQ LEQF GEQF GEQ
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIVIDE MOD TIMESF DIVIDEF
%nonassoc UMINUS
%left POW
%left POWF
%left CONS HEAD TAIL CAT LEN

%start entry
%type <expr> entry

%%
expr:
    | expr ASSIGN expr { App (App (Assign, $1), $3) }
    | expr OR expr { App (App(Or, $1), $3) }
    | expr AND expr { App (App(And, $1), $3) }
    | NOT expr { App(Not, $2) }
    | expr EQ expr { App (App(Eq, $1), $3) }
    | expr EQF expr { App (App(EqF, $1), $3) }
    | expr NEQ expr { App (App(Neq, $1), $3) }
    | expr NEQF expr { App (App(NeqF, $1), $3) }
    | expr LESS expr { App (App(Add, $1), $3) }
    | expr LESSF expr { App (App(Add, $1), $3) }
    | expr GREATER expr { App (App(Add, $1), $3) }
    | expr GREATERF expr { App (App(Add, $1), $3) }
    | expr LEQ expr { App (App(Leq, $1), $3) }
    | expr LEQF expr { App (App(LeqF, $1), $3) }
    | expr GEQ expr { App (App(Geq, $1), $3) }
    | expr GEQF expr { App (App(GeqF, $1), $3) }
    | expr PLUS expr { App (App(Add, $1), $3) }
    | expr MINUS expr { App (App (Sub, $1), $3) }
    | expr TIMES expr { App (App (Mult, $1), $3) }
    | expr DIVIDE expr { App (App(Div, $1), $3) }
    | expr PLUSF expr{ App (App(AddF, $1), $3) }
    | expr MINUSF expr { App (App(SubF, $1), $3) }
    | expr TIMESF expr{ App (App(MultF, $1), $3) }
    | expr DIVIDEF expr { App (App(DivF, $1), $3) }
    | expr POW expr { App (App(Pow, $1), $3) }
    | expr POWF expr { App (App(PowF, $1), $3) }
    | expr CONS expr { App (App(Cons, $1), $3) }
    | HEAD expr { App(Head, $2) }
    | TAIL expr { App (Tail, $2) }
    | expr CAT expr { App (App(Cat, $1), $3)}
    | LEN expr { App (Len, $2)}
    | MINUS expr %prec UMINUS { App(Neq, $2) }
    
entry:
    | expr EOF                { $1 }


