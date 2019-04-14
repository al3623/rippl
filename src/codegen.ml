module L = Llvm
open Ast
open Tast
open Lib
let rec texpr_ptr texpr = match texpr with
            | (tx, _) -> match tx with 
                | TIntLit n -> let int_ = L.const_int i32_t n in
                        L.build_call makeInt [| int_ |] "makeInt" builder
                | TFloatLit f -> let float_ = L.const_float float_t f in
                        L.build_call makeFloat [| float_ |] "makeFloat" builder
                | TBoolLit b -> let bool_ = L.const_int i1_t (if b then 1 else 0) in
                        L.build_call makeBool [| bool_ |] "makeBool" builder
                | TCharLit c -> let char_ = L.const_int i8_t (Char.code c) in
                        L.build_call makeChar [| char_ |] "makeChar" builder
                | _ -> raise (Failure "tx not implemented")
            

let translate decl_lst =
        
	(* find and execute/print main *)
  	let print_main vd = match vd with
  		| (_, (TypedVdef ("main", (TListLit(clist), TconList(Char))))) ->
			let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 2 |]
				"empty" builder in
			let rec print_string str prevlist = (match str with
		  		| (TCharLit(c),char_ty) :: xs -> 
		  			let l_char = L.const_int i8_t (Char.code c) in
		  			let charstar = L.build_call makeChar
						[| l_char |] "makeChar" builder in
					let nodestar = L.build_call makeNode
						[| charstar |] "makeNode" builder in
					let nextlist = L.build_call appendNode
						[| emptylist ; nodestar |] "appendNode" builder in
		  			print_string xs nextlist
		  		| _ ->
		  			L.build_call printPrimList [| prevlist |]
						"printPrimList" builder ;
					L.build_call printf_func [| char_format_str ; l_char |]
						"printf" builder) 
		   	in
			let _ = print_string clist emptylist in
		    L.build_ret (L.const_int i32_t 0) builder;
                    
		| (_, (TypedVdef ("main", (TListRange(start_end), TconList(Int))))) ->
			let rangelist = makerangelist start_end "mainlist" in
			let _ = L.build_call printRangeList [| rangelist |]
				"printRangeList" builder in
			let _ =	L.build_call printf_func [| char_format_str ; l_char |]
				"printf" builder in
			L.build_ret (L.const_int i32_t 0) builder
		| (_, (TypedVdef ("main", (TListLit([]), TconList(_))))) ->
			let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 0 |]
				"emptyl" builder in
			let _ = L.build_call printPrimList [| emptylist |]
				"printPrimList" builder in
			let _ =	L.build_call printf_func [| char_format_str ; l_char |]
				"printf" builder in
			L.build_ret (L.const_int i32_t 0) builder
                | (_, (TypedVdef ("main", (TIte(cond, then_expr, else_expr), typ)))) ->
                        (* evaluate condition *)
                        let condition = match cond with
                                | (TBoolLit b, _) -> b
                                | _ -> raise(Failure "not boolean") in
                        let expr_ptr = if condition then texpr_ptr then_expr else texpr_ptr else_expr in
                        let ty_code = match typ with
                                | Int -> L.const_int i32_t 0
                                | Bool -> L.const_int i32_t 1
                                | Char -> L.const_int i32_t 2
                                | Float -> L.const_int i32_t 3
                                | _ -> raise (Failure "type")
                        in
                        let _ = L.build_call printAny [| expr_ptr ; ty_code |] "printAny" builder in
                        (* print newline *)
                        let _ = L.build_call printf_func [| char_format_str ; l_char |]
                                "printf" builder in 
                        (* return 0 *)
                        L.build_ret (L.const_int i32_t 0) builder
                | _ -> raise (Failure "NO")
    in 
    let _ = print_main (List.hd decl_lst) in
  	the_module
