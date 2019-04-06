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
  					let rec print_string str b = (match str with
			  		| (TCharLit(c),char_ty) :: xs -> 
			  			let l_char = L.const_int i8_t (Char.code c) in
			  			let _ = L.build_call printf_func
							[| char_format_str ; l_char |] "printf" builder in
			  			print_string xs b
			  		| _ ->
			  			let l_char = L.const_int i8_t (Char.code '\n') in
			  			L.build_call printf_func [| char_format_str ; l_char |]
						"printf" builder) 
		    	in
			let _ = print_string clist builder in
		    L.build_ret (L.const_int i32_t 0) builder;
		| (_, (TypedVdef ("main", (TListRange(start_end), TconList(Int))))) ->
			let rangelist = makerangelist start_end "mainlist" in
			let _ = L.build_call printRangeList [| rangelist |]
				"printRangeList" builder in
			let l_char = L.const_int i8_t (Char.code '\n') in
			let _ =	L.build_call printf_func [| char_format_str ; l_char |]
				"printf" builder in
			L.build_ret (L.const_int i32_t 0) builder
  		| _ -> raise (Failure "NO")
    in 
    let _ = print_main (List.hd decl_lst) in
  	the_module
