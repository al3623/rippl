module L = Llvm
open Ast
open Tast

let translate decl_lst =
	let context = L.global_context() in
	let the_module = L.create_module context "Rippl" in
	(* int, char, bool, float, void *)
	let i32_t      	 = L.i32_type    context
	  and i8_t       = L.i8_type     context
	  (*and float_t    = L.double_type context 
	  and void_t     = L.i8_type   context*) in

	(*let ltype_of_typ = function
      Int   -> i32_t
    | Bool  -> i8_t
    | Float -> float_t
    | Char -> i8_t
  	in*)
	
	(* 
		struct Node {
			void *data;
			struct Node *next;		
		};
	*)
	let struct_node_type : L.lltype = L.named_struct_type context "Node" in
	L.struct_set_body struct_node_type 
		[| L.pointer_type i8_t ; L.pointer_type struct_node_type |] false; 
 
	(* 
		struct Tuple {
			int t1;
			int t2;
			void *first;
			void *second;
		}; 
	*) 
	let struct_tuple_type : L.lltype = L.named_struct_type context "Tuple" in
	L.struct_set_body struct_tuple_type 
		[| i32_t ; i32_t ; L.pointer_type i8_t ; L.pointer_type i8_t |] false;	

	(*
		struct Maybe {
			int ty;
			int is_none;
			void *data;
		}
	*)
	let struct_maybe_type : L.lltype = L.named_struct_type context "Maybe" in
	L.struct_set_body struct_maybe_type
		[| i32_t ; i32_t ; L.pointer_type i8_t |] false;
	
	(* declare void printf(char *,...) *)	
	let printf_t : L.lltype = 
	      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let printf_func : L.llvalue = 
	      L.declare_function "printf" printf_t the_module in

	(*
	let eval_main vd = match vd with
  		| (_, (TypedVdef ("main", (TListLit(clist), TconList (Char) ) ))) -> 
	*)
		
	(* find and execute/print main *)
  	let print_main vd = match vd with
  		| (_, (TypedVdef ("main", (TListLit(clist), TconList (Char) ) ))) -> 
  					    let main_t = L.function_type i32_t [| |] in
		    let main_f = L.define_function "main" main_t the_module in
		    let builder = L.builder_at_end context (L.entry_block main_f) in
		    let char_format_str = L.build_global_stringptr "%c" "fmt" builder in
		    	let rec print_string str b = (match str with
			  		| (TCharLit(c),char_ty) :: xs -> 
			  			let l_char = L.const_int i8_t (Char.code c) in
			  			let _ = L.build_call printf_func [| char_format_str 
						; l_char |] "printf" builder in
			  			print_string xs b

			  		| _ ->
			  			let l_char = L.const_int i8_t (Char.code '\n') in
			  			L.build_call printf_func [| char_format_str ; l_char |] "
						printf" builder) 
		    	in
			let _ = print_string clist builder in
		    L.build_ret (L.const_int i32_t 0) builder
  		| _ -> raise (Failure "NO")
    in 
    let _ = print_main (List.hd decl_lst) in
  	the_module
