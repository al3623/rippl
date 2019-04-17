module L = Llvm
open Ast
open Tast

let ty_code = function
	| Int			-> 0
	| Bool			-> 1
	| Char			-> 2
	| Float			-> 3
	| TconList _	-> 4
	| TconTuple _ 	-> 5
	| Tmaybe	_	-> 6
	| _				-> raise (Failure "no type code: unprintable")

let context = L.global_context()

let i32_t      	 = L.i32_type    context
  and i8_t       = L.i8_type     context
  and float_t    = L.double_type context 
  and void_t     = L.void_type   context
  and i1_t       = L.i1_type     context

let the_module = L.create_module context "Rippl"

let main_t = L.function_type i32_t [| |] 
let main_f = L.define_function "main" main_t the_module 

let builder = L.builder_at_end context (L.entry_block main_f)

let char_format_str = L.build_global_stringptr "%c" "fmt" builder
let l_char = L.const_int i8_t (Char.code '\n')
(* 
	struct Node {
		void *data;
		struct Node *next;		
	};
*)
let struct_node_type : L.lltype = L.named_struct_type context "Node" 
 
(* 
	struct Tuple {
		int t1;
		int t2;
		void *first;
		void *second;
	}; 
*)
let struct_tuple_type : L.lltype = L.named_struct_type context "Tuple"

(*
	struct Maybe {
		int ty;
		int is_none;
		void *data;
	}
*)
let struct_maybe_type : L.lltype = L.named_struct_type context "Maybe"

(*
	struct List {
		struct Node *head;
		struct Node *last_eval;
		int curr_index;
		int start;
		int end;
		int content_type;
		int type;
	}
*)
let struct_list_type : L.lltype = L.named_struct_type context "List"

let _ =
	L.struct_set_body struct_maybe_type
	[| i32_t ; i32_t ; L.pointer_type i8_t |] false;
	L.struct_set_body struct_tuple_type 
	[| i32_t ; i32_t ; L.pointer_type i8_t ; L.pointer_type i8_t |] false;
	L.struct_set_body struct_node_type 
	[| L.pointer_type i8_t ; L.pointer_type struct_node_type |] false;
	L.struct_set_body struct_list_type
	[| L.pointer_type struct_node_type		(* struct Node *head *) 
		; i32_t								(* int content_type *) 
		; i32_t								(* int type *)
		; L.pointer_type struct_node_type	(* struct Node *last_eval *)
		; i32_t								(* int curr_index *)
		; i32_t								(* int start *)
		; i32_t								(* int end *)
		; L.pointer_type (L.var_arg_function_type
			(L.pointer_type i8_t) 
			[| L.pointer_type i8_t |])		(* void *(expr* )(void *,...) *)
		; L.pointer_type struct_list_type	(* lists vbinds are over *)
		; L.pointer_type struct_node_type 	(* index in each list for vbinds *)
		; L.pointer_type
			(L.pointer_type (L.var_arg_function_type
			i32_t
			[| L.pointer_type i8_t |]))		(* int ( **filt)(void *,...) *)
	|] false

(* HEAP ALLOCATE PRIMS *)
let makeIntVoid_t : L.lltype =
        L.function_type (L.pointer_type i8_t) [| L.pointer_type i32_t |]
let makeIntVoid : L.llvalue =
        L.declare_function "makeIntVoid" makeIntVoid_t the_module
let makeFloatVoid_t : L.lltype =
        L.function_type (L.pointer_type i8_t) [| L.pointer_type float_t |]
let makeFloatVoid : L.llvalue =
        L.declare_function "makeFloatVoid" makeFloatVoid_t the_module

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
	L.function_type (L.pointer_type struct_list_type)
	[| i32_t |]
let makeEmptyList : L.llvalue =
	L.declare_function "makeEmptyList" makeEmptyList_t the_module
let makeemptylist ty name =
	L.build_call makeEmptyList
	[| L.const_int i32_t ty |]
	name builder

let makeInfinite_t : L.lltype =
	L.function_type (L.pointer_type struct_list_type)
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
	L.function_type (L.pointer_type struct_list_type)
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
	L.function_type (L.pointer_type struct_list_type)
	[|  i32_t |] 
let makeEmptyList : L.llvalue =
	L.declare_function "makeEmptyList" makeEmptyList_t the_module

let appendNode_t : L.lltype =
	L.function_type (L.pointer_type struct_list_type)
	[| L.pointer_type struct_list_type ; L.pointer_type struct_node_type |]
let appendNode : L.llvalue =
	L.declare_function "appendNode" appendNode_t the_module

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
	L.function_type i32_t [| L.pointer_type struct_list_type |]
let printPrimList : L.llvalue =
	L.declare_function "printPrimList" printPrimList_t the_module

let printAny_t : L.lltype =
	L.function_type void_t [| L.pointer_type i8_t ; i32_t |]
let printAny : L.llvalue =
	L.declare_function "printAny" printAny_t the_module
