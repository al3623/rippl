module L = Llvm
open Ast
open Tast
open Structs
open Lib

let eval_t : L.lltype =
	L.function_type (L.pointer_type i8_t) [| L.pointer_type struct_thunk_type |]

let add_t : L.lltype = 
	L.function_type (L.pointer_type i32_t)
		[| L.pointer_type struct_thunk_type 
		; L.pointer_type struct_thunk_type |]
let add : L.llvalue =
	L.declare_function "add" add_t the_module

let mult_t : L.lltype = 
	L.function_type (L.pointer_type i32_t)
		[| L.pointer_type struct_thunk_type 
		; L.pointer_type struct_thunk_type |]
let mult : L.llvalue =
	L.declare_function "mult" add_t the_module


