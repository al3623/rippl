module L = Llvm
open Ast
open Tast
open Structs
open Lib

let mapl_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
		[| L.pointer_type struct_thunk_type ; L.pointer_type struct_thunk_type |]
let mapl : L.llvalue =
	L.declare_function "mapl" mapl_t the_module

let filterl : L.llvalue =
	L.declare_function "filterl" mapl_t the_module

let map_listl_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
	[| L.pointer_type struct_thunk_type ; L.pointer_type struct_thunk_type |]
let map_listl : L.llvalue =
	L.declare_function "map_listl" map_listl_t the_module
