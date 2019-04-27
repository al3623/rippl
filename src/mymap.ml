module L = Llvm
open Ast
open Tast
open Structs
open Lib

let map_t : L.lltype =
	L.function_type (L.pointer_type struct_list_type)
		[| L.pointer_type struct_list_type ; L.pointer_type struct_thunk_type |]
let map : L.llvalue =
	L.declare_function "map" map_t the_module

let filter : L.llvalue =
	L.declare_function "filter" map_t the_module

let map_list_t : L.lltype =
	L.function_type (L.pointer_type struct_list_type)
	[| L.pointer_type struct_list_type ; L.pointer_type struct_list_type |]
let map_list : L.llvalue =
	L.declare_function "map_list" map_list_t the_module
