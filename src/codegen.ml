module L = Llvm
open Ast
open Tast
open Lib

let translate decl_lst =
	(*
	let eval_main vd = match vd with
  		| (_, (TypedVdef ("main", (TListLit(clist), TconList (Char) ) ))) -> 
	*)	
	
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
  		| _ -> raise (Failure "NO")
    in 
    let _ = print_main (List.hd decl_lst) in
  	the_module
