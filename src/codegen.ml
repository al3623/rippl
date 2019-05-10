module L = Llvm
open Pretty_type_print
open Ast
open Tast
open Lib
open Structs
open Thunk
open Natives
open Mymap

module StringMap = Map.Make(String)

let translate (decl_lst: (decl * typed_decl) list) =

    (* Get non-lambda Vdefs and lambda vdefs*)
    let (var_lst, lambda_lst) = 
        let islambda = function
            | (_, TypedVdef(_,(TLambda(_,_),_))) -> true
            | _ -> false
        in
        let notlambda = fun x -> not (islambda x)
        in
    (List.filter (notlambda) decl_lst, List.filter (islambda) decl_lst)
    in
    (* global map of variables: map var_name to llvalue - 
     * initialized to zero values 
     * Only non-lambdas *)
    let global_vars: L.llvalue StringMap.t = 
        let global_var m decl =
            let vdef = snd decl in
            let (name, zero) = match vdef with
                | TypedVdef(n, _) -> (n, L.const_int i32_t 0) in
            StringMap.add name (L.define_global name zero the_module) m in
        List.fold_left global_var StringMap.empty var_lst in

    (* TEST: print the global vars (not lambdas) *)
    print_endline "* * * GLOBAL VARIABLES * * *";
    StringMap.iter (fun k v -> print_endline k) global_vars;
    
    let get_ltyp tarr = match tarr with
          Tarrow(t1, t2) -> t1
        | _ -> raise (Failure "Not Tarrow")
    in

	let rec flatten_arrow_type = function
		| Tarrow(l,r) -> l :: (flatten_arrow_type r)
		| r -> [r]
	in

	let rec lambda_var_list = function
		| (TLambda(v,ex),_) -> v::(lambda_var_list ex)
		| _ -> []
	in

	let load_deref_args args eval_builder n =
		let rec helper index = 
			if index = n 
			then [] 
			else ((
				let tmp = L.build_gep struct_thunk 
				[| args; L.const_int i32_t n; L.const_int i32_t 0 |] 
				"tmp" eval_builder in
				let arg_n = L.build_load tmp "arg" eval_builder in
				arg_n
			) :: (helper (index + 1)))
		in helper 0
	in

	let build_eval_func_body eval_dcls = function
		| (_,TypedVdef(name,(txpr,Tarrow(l,r)))) ->
			let eval_decl = (match (StringMap.find_opt ("$eval_"^name) eval_dcls) with
				| Some (decl,_) -> decl
				| None -> raise (Failure ("No eval function for decl
"^name))) in
			let eval_builder = 
				L.builder_at_end context (L.entry_block eval_decl) in
			let ts = L.params eval_decl  in
		(*	let tmp = L.build_gep struct_thunk 
				[| t; L.const_int i32_t 0; L.const_int i32_t 3 |] 
				"tmp" eval_builder in
			let args = L.build_load tmp "args" eval_builder in
			let types = l :: (flatten_arrow_type r) in
			let num_args = List.length types in
			let ordered_args = load_deref_args args eval_builder num_args in 
			let func = (match (L.lookup_global ("$$"^name) the_module) with
				| Some f -> f
				| None -> raise (Failure ("no matching vdef for eval "^name)))in
			let result = L.build_call func 
				(Array.of_list ordered_args) "result" eval_builder in
			ignore(L.build_ret result eval_builder) *) ()
		| _ -> () (* this is not a function decl but a global thingy *)
	in

	let stack_alloc builder var argll =
		let _ = print_endline "YUUPfuck" in
		let stack_ref = L.build_alloca 
			(L.pointer_type struct_thunk_type) var builder in
		(*let _ = L.build_store argll stack_ref builder in stack_ref*) ()
	in

	(* GIVE THIS fn_decls *)
	let build_func_body func_decls = function
		| (_,TypedVdef(name,(txpr,Tarrow(l,r)))) -> print_endline "YUP";
			let fn_decl = (match(StringMap.find_opt ("$$"^name) func_decls) with
				| Some (decl,_) -> decl
				| None -> raise (Failure ("No function for decl $$"^name))) in
			let fn_builder = L.builder_at_end context (L.entry_block fn_decl) in
			let vars = lambda_var_list (txpr,Tarrow(l,r)) in
			let argsll = L.params fn_decl in
			let argslll = Array.to_list argsll in
			let _ = print_endline
				("what " ^ (string_of_int (List.length argslll))) in
			let var_to_argsll_map = List.fold_left2 (fun map var ll -> 
				StringMap.add var ll map) StringMap.empty vars argslll in
			let _ = print_endline
				("the " ^ (string_of_int (StringMap.cardinal var_to_argsll_map))) in
			let var_to_local_map =
				StringMap.mapi (stack_alloc fn_builder) var_to_argsll_map
			in
			let _ = print_endline
				("fuck " ^ (string_of_int (StringMap.cardinal
var_to_local_map))) in
			(* build_expr txpr fn_builder var_to_argsll_map *) 
			()
		| _ -> ()
	in

    (* convert Tlambda -> Tlambda_def *)
    let tldef_convert (tlambda: typed_expr) (name: string) = 
        match tlambda with
        | (TLambda(var, e), typ) ->
            {   
                tlname = name; 
                tltyp = (get_ltyp typ);
                trtyp = snd e;
                tlexp = (TVar var, get_ltyp typ); 
                trexp = e;
            }
        | _ -> raise (Failure "not Tlambda")
    in

    (* list of tlambda_def's *)
    let lm_defs: (tlambda_def list) =
        let to_lmdef (dec: decl * typed_decl) = match dec with
            | (_,TypedVdef(n,l)) -> tldef_convert l n
        in
    List.map to_lmdef lambda_lst
    in

    (* convert to llvm type *)
    let rec ltyp_of_typ (typ: ty) = match typ with
          Int -> L.pointer_type i32_t
        | Bool -> L.pointer_type i1_t
        | Char -> L.pointer_type i8_t
        | Float -> L.pointer_type float_t
        
        | _ -> L.pointer_type i8_t (* FIX LATER *)
    
    in

    (* get array of types of lambda *)
    let rec arg_types (lmd: tlambda_def) =
        let first_arg = [| ltyp_of_typ lmd.tltyp |] in
        let rec get_args texp = match texp with
                  (TLambda(_,txp), ty) ->
                      let typ = ltyp_of_typ (get_ltyp ty) in
                      Array.append [| typ |] (get_args txp)
                | _ -> [||]
        in
        let other_args = get_args lmd.trexp in
        Array.append first_arg other_args
    
    in
    let eval_decls: (L.llvalue * tlambda_def) StringMap.t =
        (* declare eval functions and put into a map*)
        let gen_decls m (lm_def: tlambda_def) = 
            (*(* eval function: void *f(struct Thunk*) *)
            let eval_name = "$eval_" ^ lm_def.tlname in
            StringMap.add eval_name (L.define_function eval_name eval_func_type 
                 the_module, lm_def) m
    in List.fold_left gen_decls StringMap.empty lm_defs
    in

    print_endline "* * * FUNCTIONS * * *";
    
    let fn_decls: (L.llvalue *tlambda_def) StringMap.t =
        let gen_decls m (lm_def: tlambda_def) =
            (* core function declaration *)
            let fname = lm_def.tlname in
            let fn_args = arg_types lm_def in
           
            (* TEST: print number of args *) 
            let _ = print_endline ("number of args of " ^ fname ^ " is " ^
                (string_of_int (Array.length fn_args))) in
            
            (* core function: void *f(...)  *)
            let ftype = L.function_type (L.pointer_type struct_thunk_type) fn_args in
            StringMap.add ("$$"^fname) (L.define_function ("$$"^fname) ftype the_module,
                lm_def) m
    in List.fold_left gen_decls StringMap.empty lm_defs
    in

    (* TEST - print the function names*)
    StringMap.iter (fun k v -> print_endline k) eval_decls;
    StringMap.iter (fun k v -> print_endline k) fn_decls;


    (* fn to add terminal instruction if needed *) 
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
              Some _ -> ()
            | None -> ignore (instr builder) 
    in
    
    let thunks: L.llvalue StringMap.t =
        let declare_thunk m (lmd: tlambda_def) =
            let name = "$thunk_"^lmd.tlname in
                let lval = L.declare_global struct_thunk_type name the_module in
                    StringMap.add name lval m
        in
        List.fold_left declare_thunk StringMap.empty lm_defs 
    in

    (* build thunk for each function and put into map *)
    
    let build_thunk (lmd: tlambda_def) =
        (* get number of args of function *)
        let rec argnum texp = match texp with
              (TLambda(_,t),_) -> 1 + (argnum t)
            | _ -> 0
        in
        let argc = 1 + (argnum lmd.trexp) in
        let eval_name = "$eval_" ^ lmd.tlname in
        let eval_fn = fst (StringMap.find eval_name eval_decls) in
        let num_args = L.const_int i32_t argc in
        let thunk_name = "$$" ^ lmd.tlname in

        let f_init_thunk = L.define_global (thunk_name ^ "_init_thunk") 
            (L.const_null struct_thunk_type) the_module in

        let _ = L.build_call initThunk [| f_init_thunk ; eval_fn; num_args |] 
            "initThunk" builder in
        print_endline "BUILD_THUNK"
        
    in

    List.iter build_thunk lm_defs;
    
    let rec build_expr (texp: typed_expr) builder (scope: L.llvalue StringMap.t) =
        (* convert tx to llvm pointer *)
        let make_ptr t = match t with
              TIntLit i -> L.build_call makeInt 
                [| L.const_int i32_t i |] "int" builder
            | TFloatLit f -> L.build_call makeFloat 
                [| L.const_float float_t f |] "float" builder
            | TCharLit c -> L.build_call makeChar
                [| L.const_int i8_t (Char.code c) |] "char" builder
            | TBoolLit b -> let x = if b then 1 else 0 in
                L.build_call makeBool [| L.const_int i8_t x |] "bool" builder

            | _ -> raise (Failure "make_ptr")
        in

        let tex = fst texp in 
        let typ = snd texp in match tex with
            (* literals - build thunk literals *)
              TIntLit n -> L.build_call makeInt [| L.const_int i32_t n |] 
                "makeInt" builder
            | TFloatLit f -> L.build_call makeFloat [| L.const_float float_t f |] 
                "makeFloat" builder
            | TCharLit c -> L.build_call makeChar [| L.const_int i8_t (Char.code c) |] 
                "makeChar" builder
            | TBoolLit b -> L.build_call makeBool [| L.const_int i8_t (if b then 1 else 0) |] 
                "makeBool" builder
            | TVar s -> (match (StringMap.find_opt s scope) with
                  Some lval -> lval
                | None -> (match (StringMap.find_opt s global_vars) with
                      Some l -> l
                    | None -> (match (StringMap.find_opt s fn_decls) with
                          Some l_ -> fst l_
                        | None -> raise (Failure (s^ " not found in scope"))
                            ))
            )
            | (TListRange(start_end)) -> makerangelist start_end "mainlist" 
            | TListLit texlst -> 
                let ty_code = match typ with
                         TconList Int -> 0
                       | TconList Bool -> 1
                       | TconList Char -> 2
                       | TconList Float -> 3
                       | _ -> raise (Failure "ty_code")
                in
                let emptylist = L.build_call makeEmptyList [| L.const_int i32_t ty_code |]
                "empty" builder in
                
                let makestar texp = match texp with
                          (TCharLit c ,_) -> let _char = L.const_int i8_t (Char.code c) in
                                L.build_call makeChar [| _char |] "makeChar" builder
                        | (TIntLit i, _) -> let _int = L.const_int i32_t i in
                                L.build_call makeInt [| _int |] "makeInt" builder
                        | (TBoolLit b, _) -> let _bool = L.const_int i8_t (if b then 1 else 0) in
                                L.build_call makeBool [| _bool |] "makeBool" builder
                        | (TFloatLit f, _) -> let _float = L.const_float float_t f in
                                L.build_call makeFloat [| _float |] "makeFloat" builder
                        
                        | _ -> raise (Failure "makestar")
                in
                let rec build_list s prevlist = (match s with
                    | h :: t -> let estar = makestar h in
                        let nodestar = L.build_call makeNode
                            [| estar |] "makeNode" builder in
                        let nextlist = L.build_call appendNodeThunk
                            [| prevlist ; nodestar |] "appendNodeThunk" builder in
                        build_list t nextlist
                    | [] -> prevlist
                )
                in
                build_list texlst emptylist

            | TLet (ta, t) -> (match ta with
                                  TAssign (s, te) -> let v1 = build_expr te builder scope in
                                        let new_scope = StringMap.add s v1 scope in
                                        build_expr t builder new_scope 
            )
            (* Application *)
            | TApp(t1, t2) as tapp -> let lv1 = build_expr t1 builder scope in
                let lv2 = build_expr t2 builder scope in
                L.build_call apply [| lv1; lv2 |] "apply" builder

            | TIte(cond, then_ex, else_ex) ->
                let cond_ = build_expr cond builder scope in
                let then_ = build_expr then_ex builder scope in
                let else_ = build_expr else_ex builder scope in
                L.build_call makeIte [| cond_; then_; else_ |] "ifthenelse" builder

            | TAdd -> add_init_thunk
            | TSub -> sub_init_thunk       
            | TMult -> mult_init_thunk
            | TDiv -> divi_init_thunk
            | TMod -> mod_init_thunk
            | TPow -> powe_init_thunk
            | TEq -> eq_init_thunk
            | TNeq -> neq_init_thunk
            | TGeq -> geq_init_thunk
            | TLeq -> leq_init_thunk
            | TLess -> less_init_thunk
            | TGreater -> greater_init_thunk
            | TNeg -> neg_init_thunk
            | TAddF -> addf_init_thunk
            | TSubF -> subf_init_thunk
            | TMultF -> multf_init_thunk
            | TDivF -> divf_init_thunk
            | TPowF -> powef_init_thunk
            | TEqF -> eqf_init_thunk
            | TNeqF -> neqf_init_thunk
            | TGeqF -> geqf_init_thunk
            | TLeqF -> leqf_init_thunk
            | TLessF -> lessf_init_thunk
            | TGreaterF -> greaterf_init_thunk
            | TNegF -> negf_init_thunk
            | TCons -> cons_init_thunk
            | TCat -> cat_init_thunk
            | TLen -> length_init_thunk
            | THead -> head_init_thunk
            | TTail -> tail_init_thunk
            | TFirst -> first_init_thunk
            | TSec -> second_init_thunk
            | TLambda(_, _) -> raise(Failure "unexpected lambda")

    in
    (* build eval function body *)
    (* let build_evalfn_body (lm_def: tlambda_def) =
        let (eval_fn, _) = StringMap.find ("$eval_"^ lm_def.tlname) 
                eval_decls in
        let builder = L.builder_at_end context (L.entry_block eval_fn) in
        
        (* get arg values from thunk struct *)

        (* call the core function *)

        (* return the result *)

        let fn_builder = builder in
        add_terminal fn_builder (L.build_ret (L.const_pointer_null 
                (L.pointer_type i8_t)))
    in*)
    (* build core fn body *)
    let build_fn_body (lm_def: tlambda_def) = 
        let (fn, _) = StringMap.find (lm_def.tlname) fn_decls in
        let builder = L.builder_at_end context (L.entry_block fn) in
        
        (*
        let locals = 
            let add_formal m (t, n) llval = 
                L.set_value_name n llval;
                let local = L.build_alloca (ltyp_of_typ t) n builder in
                ignore (L.build_store p local builder)
                StringMap.add n local m;
        *)

        (* get final expression in function
        let e = 
            let rec fin_ex = function
              TLambda(_,(t,_)) -> fin_ex t
            | x -> x
        in fin_ex (fst lm_def.trexp) 
        in
*)
        let fn_builder = builder in
        add_terminal fn_builder (L.build_ret (L.const_pointer_null 
                (L.pointer_type i8_t)))
    in
    
    let print_expr (lv: L.llvalue) (vtype: ty) =
        (* call invoke on thunk *)
        let _ = L.build_call invoke [| lv |] "invoke" builder in
        (* print *)
        let _ = (match vtype with
            | TconList(t) -> (match t with
                | Int | Bool| Float | Char -> L.build_call printPrimList [| lv |] "" builder
                | _ -> raise (Failure "what the fuck kind of list is this"))
            | Int -> L.build_call printAnyThunk [| lv ; L.const_int i32_t 0 |] "" builder
            | Bool -> L.build_call printAnyThunk [| lv ; L.const_int i32_t 1 |] "" builder
            | Float -> L.build_call printAnyThunk [| lv ; L.const_int i32_t 3 |] "" builder
            | Char -> L.build_call printAnyThunk [| lv ; L.const_int i32_t 2 |] "" builder
            | ty -> raise(Failure("Main is of unprintable type"^(ty_to_str ty)))
        ) in
        L.build_call printf_func [| char_format_str ; L.const_int i8_t (Char.code('\n')) |] "printf" builder


    in

    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef("main",texp)) -> (*
                (* build expr *)
                let v = build_expr texp builder StringMap.empty in
                (* print expr if main*)
                ignore (print_expr v (snd texp)) *) ()
			| (_, TypedVdef(name,(tex,Tarrow(_)))) as tup-> 
				build_func_body fn_decls tup; ()
				(* build_func_body *)
    in
    let _ = List.iter build_decl decl_lst in
        ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module
