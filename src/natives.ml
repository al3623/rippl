module L = Llvm
open Ast
open Tast
open Structs
open Lib

let eval_t : L.lltype =
	L.function_type (L.pointer_type i8_t) [| L.pointer_type struct_thunk_type |]

let arith_t : L.lltype =
	L.function_type (L.pointer_type i32_t)
		[| L.pointer_type struct_thunk_type 
		; L.pointer_type struct_thunk_type |]

let add : L.llvalue =
	L.declare_function "add" arith_t the_module
	
let add_eval : L.llvalue =
	L.declare_function "add_eval" eval_t the_module

let sub : L.llvalue =
	L.declare_function "sub" arith_t the_module

let sub_eval : L.llvalue =
	L.declare_function "sub_eval" eval_t the_module

let mult : L.llvalue =
	L.declare_function "mult" arith_t the_module

let mult_eval : L.llvalue =
	L.declare_function "mult_eval" eval_t the_module

let neq : L.llvalue =
	L.declare_function "neq" arith_t the_module

let neq_eval : L.llvalue =
	L.declare_function "neq_eval" eval_t the_module

let addf_eval : L.llvalue =
	L.declare_function "addf_eval" eval_t the_module

let addf : L.llvalue =
	L.declare_function "addf" arith_t the_module


