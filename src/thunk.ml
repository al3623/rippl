module L = Llvm
open Ast
open Tast
open Lib

(* 
	struct Thunk {
		struct Thunk *( *f)(struct Thunk *,void * );
		int num_args;
		int filled_args;
		void **args;
		void *value;
	};
*)
let struct_thunk_type : L.lltype = L.named_struct_type context "Thunk" 

let call_func_type : L.lltype = L.function_type (L.pointer_type struct_thunk_type)
	[| L.pointer_type struct_thunk_type ; L.pointer_type i8_t |]
 
let _ =
	L.struct_set_body struct_thunk_type
	[| L.pointer_type struct_node_type		
		(* struct Thunk *( *f)(struct Thunk *, void * )*)
		;(L.pointer_type call_func_type)
		; i32_t									(* int num_args *) 
		; i32_t									(* int filled_args *)
		; L.pointer_type (L.pointer_type i8_t)	(* void **args *)
		; L.pointer_type i8_t					(* void *value *)
	|] false

let initThunk_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
		[| L.pointer_type call_func_type ; L.pointer_type i8_t|]
let initThunk : L.llvalue =
	L.declare_function "init_thunk" initThunk_t the_module

let invoke : L.llvalue =
	L.declare_function "invoke" call_func_type the_module
