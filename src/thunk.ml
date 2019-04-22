module L = Llvm
open Ast
open Tast
open Structs
open Lib

let initThunk_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
		[| L.pointer_type call_func_type ; L.pointer_type i8_t|]
let initThunk : L.llvalue =
	L.declare_function "init_thunk" initThunk_t the_module

let initThunkLiteral_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
		[| L.pointer_type i8_t |]
let initThunkLiteral : L.llvalue =
	L.declare_function "init_thunk_literal" initThunkLiteral_t the_module

let invoke : L.llvalue =
	L.declare_function "invoke" call_func_type the_module
