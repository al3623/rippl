module L = Llvm
open Ast
open Tast
open Structs

let ty_code = function
	| Int			-> 0
	| Bool			-> 1
	| Char			-> 2
	| Float			-> 3
	| TconList _	        -> 4
	| TconTuple _ 	        -> 5
	| Tmaybe _	        -> 6
	| _			-> raise (Failure "no type code: unprintable")


let the_module = L.create_module context "Rippl"

let main_t = L.function_type i32_t [| |] 
let main_f = L.define_function "main" main_t the_module 

let builder = L.builder_at_end context (L.entry_block main_f)

let char_format_str = L.build_global_stringptr "%c" "fmt" builder
let int_format_str = L.build_global_stringptr "%d" "fmt_int" builder
let float_format_str = L.build_global_stringptr "%f" "fmt_float" builder

let l_char = L.const_int i8_t (Char.code '\n')

(* HEAP ALLOCATE PRIMS *)
let makeInt_t : L.lltype =
	L.function_type (L.pointer_type i32_t) [| i32_t |]
let makeInt : L.llvalue =
	L.declare_function "makeInt" makeInt_t the_module
let makeBool_t : L.lltype =
	L.function_type (L.pointer_type i8_t) [| i1_t |]
let makeBool : L.llvalue =
	L.declare_function "makeBool" makeBool_t the_module
let makeChar_t : L.lltype =
	L.function_type (L.pointer_type i8_t) [| i8_t |]
let makeChar : L.llvalue =
	L.declare_function "makeChar" makeChar_t the_module
let makeFloat_t : L.lltype =
	L.function_type (L.pointer_type float_t) [| float_t |]
let makeFloat : L.llvalue =
	L.declare_function "makeFloat" makeFloat_t the_module

(* HEAP ALLOCATE TUPLES *)
let makeTuple_t : L.lltype =
	L.function_type (L.pointer_type struct_tuple_type) 
	[| L.pointer_type i8_t ; L.pointer_type i8_t ; i32_t ; i32_t |]
let makeTuple : L.llvalue =
	L.declare_function "makeTuple" makeTuple_t the_module

(* HEAP ALLOCATE MAYBES *)
let makeMaybe_t : L.lltype =
	L.function_type (L.pointer_type struct_maybe_type) 
	[| L.pointer_type i8_t ; i32_t |]
let makeMaybe : L.llvalue =
	L.declare_function "makeMaybe" makeMaybe_t the_module

(* HEAP ALLOCATE LIST STRUCTS *)
let makeNode_t : L.lltype =
	L.function_type (L.pointer_type struct_node_type)
	[| L.pointer_type i8_t |]
let makeNode : L.llvalue =
	L.declare_function "makeNode" makeNode_t the_module
(* TODO: make node *)

let makeEmptyList_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
	[| i32_t |]
let makeEmptyList : L.llvalue =
	L.declare_function "makeEmptyList" makeEmptyList_t the_module
let makeemptylist ty name =
	L.build_call makeEmptyList
	[| L.const_int i32_t ty |]
	name builder

let makeInfinite_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
	[| i32_t |]
let makeInfinite : L.llvalue =
	L.declare_function "makeInfinite" makeInfinite_t the_module
let makeinfinite start name = match start with
	| (TIntLit s, Int) -> 
		L.build_call makeInfinite
		[| L.const_int i32_t s |] 
		name builder
	| _ -> raise (Failure "type error in infinite list")

let makeRangeList_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
	[| i32_t ; i32_t |]
let makeRangeList : L.llvalue =
	L.declare_function "makeRangeList" makeRangeList_t the_module
let makerangelist start_end name = match start_end with
	| ((TIntLit start, Int), (TIntLit fin, Int)) ->
		L.build_call makeRangeList 
		[| L.const_int i32_t start ; L.const_int i32_t fin |] 
		name builder
	| _ -> raise (Failure "type error in range list")

let makeEmptyList_t : L.lltype =
	L.function_type (L.pointer_type struct_thunk_type)
	[|  i32_t |] 
let makeEmptyList : L.llvalue =
	L.declare_function "makeEmptyList" makeEmptyList_t the_module

let appendNode_t : L.lltype =
	L.function_type (L.pointer_type struct_list_type)
	[| L.pointer_type struct_list_type ; L.pointer_type struct_node_type |]
let appendNode : L.llvalue =
	L.declare_function "appendNode" appendNode_t the_module
let appendNodeThunk_t : L.lltype = 
	L.function_type (L.pointer_type struct_thunk_type)
	[| L.pointer_type struct_thunk_type ; L.pointer_type struct_node_type |]
let appendNodeThunk : L.llvalue =
	L.declare_function "appendNodeThunk" appendNodeThunk_t the_module

(* PRINTING *)	
let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module 

let printRangeList_t : L.lltype =
	L.function_type i32_t [| L.pointer_type struct_list_type |]
let printRangeList : L.llvalue =
	L.declare_function "printRangeList" printRangeList_t the_module

let printPrimList_t : L.lltype =
	L.function_type i32_t [| L.pointer_type struct_thunk_type |]
let printPrimList : L.llvalue =
	L.declare_function "printPrimList" printPrimList_t the_module

let printAny_t : L.lltype =
	L.function_type void_t [| L.pointer_type i8_t ; i32_t |]
let printAny : L.llvalue =
	L.declare_function "printAny" printAny_t the_module

let printBool_t : L.lltype =
	L.function_type void_t [| i8_t |]
let printBool : L.llvalue =
	L.declare_function "printBool" printBool_t the_module

