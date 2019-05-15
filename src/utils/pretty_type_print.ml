open Ast

let rec ast_to_str exp =
    match exp with
    | Ite(e1,e2,e3) -> "if (" ^ (ast_to_str e1) ^ 
					")\nthen (" ^ (ast_to_str e2) ^ 
					")\nelse(" ^ (ast_to_str e3) ^ ")"
    | Let(e1,e2) -> "let (" ^ (assign_to_str e1) ^ 
					")\nin (" ^ (ast_to_str e2) ^ ")"
    | Lambda(e1,e2) -> "fun " ^ (e1) ^ " -> \n(" ^ (ast_to_str e2) ^ ")"
    | App(App(op, arg1), arg2) -> "((" ^ op_to_str op ^ ")~" 
		^ (ast_to_str arg1) ^ ")~" ^ (ast_to_str arg2)
    | App(op, e) ->  "(" ^ (op_to_str op) ^ ")~" ^ (ast_to_str e)
    | Var(s) -> s
	| Tuple(a,b) -> "("^(ast_to_str a)^", "^(ast_to_str b)^")"
    (* Lists *)
    | ListLit((Ast.CharLit c) :: tl) -> "\"" ^ (char_list_to_str ((Ast.CharLit c) :: tl)) ^ "\""
    | ListLit(alist) -> "[" ^ (list_to_str alist) ^ "]"
    | ListRange(e1, e2) -> "[" ^ (ast_to_str e1) ^ "..." ^ (ast_to_str e2) ^ "]"
    | ListComp(e, c) -> "[" ^ (ast_to_str e) ^ "|" ^ (clauses_to_str c) ^ "]"

    | BoolLit(b) -> string_of_bool b
    | CharLit(c) -> String.make 1 c
    | IntLit n -> string_of_int n
    | FloatLit f -> string_of_float f
    | WildCard -> "_"
    | _ -> ""

and assign_to_str = function | Assign (e1,e2)
    -> (e1) ^ "=" ^ (ast_to_str e2)

and clauses_to_str clauses =
   match clauses with
   | [Filter(e)] -> "Filter(" ^ (ast_to_str e) ^"), "
   | [ListVBind(s, l)] -> "(" ^ s ^ " over " ^ "[" ^ (ast_to_str l) ^ "]), "
   | h::t -> (clauses_to_str [h]) ^ clauses_to_str t
   | [] -> ""

and op_to_str s =
    match s with
    (* Boolean Operators *)
    | Or -> "or"
    | And -> "and"
    | Not -> "not"
    | Eq -> "=="
    | EqF -> "==."
    | Neq -> "!="
    | NeqF -> "!=."
    | Less -> "<"
    | LessF -> "<."
    | Greater -> ">"
    | GreaterF -> ">."
    | Leq -> "<="
    | LeqF -> "<=."
    | Geq -> ">="
    | GeqF -> ">=."
    (* Math Operations *)
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | AddF -> "+."
    | SubF -> "-."
    | MultF -> "*."
    | DivF -> "/."
    | Pow -> "^"
    | PowF -> "^."
    | Neg -> "neg"
    (* List Operations *)
    | Cons -> "cons"
    | Cat -> "cat"
    | Head -> "head"
    | Tail -> "tail"
    | Len -> "len"
    | _ -> ast_to_str s

and ty_to_str ty =
    match ty with
    | Bool -> "Bool"
    | Int -> "Int"
    | Char -> "Char"
    | Float -> "Float"
    | TconList(t) -> "["^ (ty_to_str t) ^ "]"
    | TconTuple(t1,t2) -> "(" ^ (ty_to_str t1) ^ "," ^ (ty_to_str t2) ^ ")"
    | Tmaybe(t) -> "Maybe " ^ (ty_to_str t) ^ ")"
    | Tvar(t) -> t
    | Tarrow(t1,t2) -> (nestarrow t1) ^ " -> " ^ (ty_to_str t2)
    | Tforall(_,t) -> ty_to_str t

and nestarrow ty =
	match ty with
	| Tarrow(t,t1) -> "("^(nestarrow t)^" -> "^(ty_to_str t1)^")"
	| el -> ty_to_str el


and char_list_to_str cl =
    match cl with
    | [] -> ""
    | [Ast.CharLit c] -> String.make 1 c
    | charlist -> let convert x = match x with 
        | Ast.CharLit c -> c 
        | _ -> 'F'
        in List.fold_left (^) "" (List.map (String.make 1) (List.map convert charlist))

and list_to_str el = match el with
        | [] -> ""
        | hd::tl ->  List.fold_left (fun curr_str e -> curr_str ^ (ast_to_str e) ^ ",") "" el
let rec print_annot_pairs lst = match lst with
	| (Annot(n1, t), Vdef(n2, e)) :: tl ->
		print_endline ("a_name: " ^ n1 ^ ", v_name: " ^ n2 ^ ", type:" ^ (ty_to_str t));
		print_annot_pairs tl
	| [] -> print_endline "done"
	| _ -> print_endline "what" 
