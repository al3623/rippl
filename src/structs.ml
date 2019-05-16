module L = Llvm

let context = L.global_context()

let i32_t      	 = L.i32_type    context
  and i8_t       = L.i8_type     context
  and float_t    = L.float_type  context 
  and void_t     = L.void_type   context
  and i1_t       = L.i1_type     context


(* 
	struct Thunk {
		void *( *eval)(struct Thunk * );
		int num_args;
		int filled_args;
		struct Thunk **args;
		void *value;
                int is_ite; 
	};
*)
let struct_thunk_type : L.lltype = L.named_struct_type context "Thunk" 

let call_func_type : L.lltype = L.function_type (L.pointer_type struct_thunk_type)
	[| L.pointer_type struct_thunk_type ; L.pointer_type struct_thunk_type |]
	(* [| L.pointer_type struct_thunk_type ; L.pointer_type i8_t |] *)

let eval_func_type : L.lltype = L.function_type (L.pointer_type i8_t)
	[|  L.pointer_type struct_thunk_type |] 
(* 
	struct Node {
		struct Thunk *data;
		struct Node *next;		
	};
*)
let struct_node_type : L.lltype = L.named_struct_type context "Node" 
 
(* 
	struct Tuple {
		int t1;
		int t2;
		struct Thunk *firs;
		struct Thunk *second;
	}; 
*)
let struct_tuple_type : L.lltype = L.named_struct_type context "Tuple"

(*
	struct Maybe {
		int ty;
		int is_none;
		struct Thunk *data;
	}
*)
let struct_maybe_type : L.lltype = L.named_struct_type context "Maybe"

(*
	struct List {
		struct Node *head;
		int content_type;
		int type;

		struct Node *last_eval;
		int curr_index;
		int start;
		int end;
	
		void * ( * expr) void *,...);
		struct List *listvbinds;
		struct Node *indexes;
		int ( **filt)( void *,...);
		int num_vbinds;
	}
*)
let struct_list_type : L.lltype = L.named_struct_type context "List"

let _ =
	L.struct_set_body struct_maybe_type
	[| i32_t ; i32_t ; L.pointer_type struct_thunk_type |] false;
	L.struct_set_body struct_tuple_type
	[| i32_t ; i32_t 
		; L.pointer_type struct_thunk_type
		; L.pointer_type struct_thunk_type |] false;
	L.struct_set_body struct_node_type 
	[| L.pointer_type struct_thunk_type 
		; L.pointer_type struct_node_type |] false;
	L.struct_set_body struct_list_type
	[| L.pointer_type struct_node_type
		; i32_t
		; i32_t
		; L.pointer_type struct_node_type
		; i32_t
		; i32_t
		; i32_t
		|] false

let _ =
	L.struct_set_body struct_thunk_type
	[| 	(* (void *( *eval)(struct Thunk * ) *)
		(L.pointer_type eval_func_type)	
		; i32_t									(* int num_args *) 
		; i32_t									(* int filled_args *)
		; L.pointer_type 
			(L.pointer_type struct_thunk_type)	(* struct Thunk **args *)
		; L.pointer_type i8_t
                ; i32_t                                        (* void *value *)
	|] false



