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

    let rec throw_away_lambda = function
        | (TLambda(_, b), _) -> throw_away_lambda b
        | other -> other
    in
    
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
    
    (* fn to add terminal instruction if needed *) 
    let add_terminal builder instr =
        match L.block_terminator (L.insertion_block builder) with
              Some _ -> ()
            | None -> ignore (instr builder) 
    in

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
	let stack_alloc builder var argll =
		let stack_ref = L.build_alloca 
			(L.pointer_type struct_thunk_type) var builder in
		let _ = L.build_store argll stack_ref builder in 
        let loaded = L.build_load stack_ref "amanda" builder in
        loaded
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

    (* get array of types of lambda *)
    let rec arg_types (lmd: tlambda_def) =
        let first_arg = [| L.pointer_type struct_thunk_type |] in
        let rec get_args texp = match texp with
                  (TLambda(_,txp), ty) ->
                      let typ = (L.pointer_type struct_thunk_type) in
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

    let fn_decls: (L.llvalue *tlambda_def) StringMap.t =
        let gen_decls m (lm_def: tlambda_def) =
            (* core function declaration *)
            let fname = lm_def.tlname in
            let fn_args = arg_types lm_def in
            
            (* core function: void *f(...)  *)
            let ftype = L.function_type (L.pointer_type i8_t) fn_args in
            StringMap.add fname (L.define_function fname ftype the_module,
                lm_def) m
    in List.fold_left gen_decls StringMap.empty lm_defs
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
            "initThunk" builder in ()
        
    in

    List.iter build_thunk lm_defs;

    let build_eval_func_body eval_dcls = function
            | (_,TypedVdef(name,(txpr,Tarrow(l,r)))) ->
                    let eval_decl = (match (StringMap.find_opt 
                    ("$eval_"^name) eval_dcls) with
                            | Some (decl,_) -> decl
                            | None -> raise (Failure ("No eval function for decl "^name))) in
                    let builder = 
                            L.builder_at_end context (L.entry_block eval_decl) in
                    
                    let types = l :: (flatten_arrow_type r) in
                    let num_args = List.length types in
                    let ts = L.params eval_decl in
                    let t = List.hd (Array.to_list ts) in

                    (* allocate thunk, args, and returned val *)
                    let p = L.build_alloca (L.pointer_type struct_thunk_type) "pthunk" builder in
                    
                    (* list of arg thunks to allocate *)
                    let rec build_arg_alloca n = if n < 1 then [] else match n with
                            | 1 -> [(L.build_alloca (L.pointer_type struct_thunk_type) 
                            ("thunk1") builder)]
                            | m -> (L.build_alloca (L.pointer_type struct_thunk_type) 
                                    ("thunk"^(string_of_int (m))) builder)::(build_arg_alloca (m-1))
                    in
                    let args = List.rev (build_arg_alloca (num_args-1))
                    in
                    let ret = L.build_alloca (L.pointer_type i8_t) "ret" builder in

                    ignore(L.build_store t p builder);

                    (* load and dereference to get the args of the thunk *)
                    let load_store_arg a ind =
                        let tload = L.build_load p "tload" builder in
                        let args = L.build_gep tload 
                        [| L.const_int i32_t 0; L.const_int i32_t 3 |] "args" builder
                        in
                        let loadargs = L.build_load args "loadargs" builder in
                        let arg = L.build_gep loadargs [| 
                            L.const_int i32_t ind |] "args" builder
                        in
                        let loadarg = L.build_load arg "loadarg" builder in
                        ignore(L.build_store loadarg a builder)
                    in

                    let rec load_store_args ind lst = match lst with
                        | [] -> ()
                        | h::t -> load_store_arg h ind; load_store_args (ind+1) t
                    in

                    load_store_args 0 args;
                    (* load all args into list *)

                    let loaded_args: (L.llvalue list) = 
                        let rec load_arg arglst = match arglst with
                            | [] -> []
                            | h::t -> (L.build_load h "load" builder) ::
                                (load_arg t)
                    in
                    load_arg args
                    in

                    (* call the function *)
                    let f = StringMap.find_opt name fn_decls in
                    let func = match f with
                            | Some f -> fst f
                            | None -> raise (Failure "function not found")
                    in
                    let result = L.build_call func (Array.of_list loaded_args) "result" builder in

                    (* store, load, cast, and return *)
                    ignore(L.build_store result ret builder);
                    let retload = L.build_load ret "retload" builder in
                    let ret_cast = L.build_bitcast retload (L.pointer_type i8_t) "ret_cast" builder in
                    L.build_ret ret_cast builder
            | (_,TypedVdef(name, _)) -> raise(Failure (name ^ "is not an arrow type!"))
    in

    let rec build_expr (texp: typed_expr) builder (scope: L.llvalue StringMap.t) =

		let ty_to_int = (function
			| Int -> 0
			| Float -> 1
			| Char -> 2
			| Bool -> 3
			| (TconList _) -> 4
			| (TconTuple _) -> 5
			| (Tmaybe _) -> 6
			|  _ -> -1) 
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
                  Some lval -> 
                    let p = L.build_alloca (L.pointer_type struct_thunk_type) "hans" builder in
                    ignore(L.build_store lval p builder);
                    let hans_load = L.build_load p "loaded_hans" builder in
                    hans_load
                | None -> (match (L.lookup_global ("$$" ^ s ^ "_init_thunk") the_module ) with
                      Some l -> l
                    | None -> (match (StringMap.find_opt s fn_decls) with
                          Some l_ -> fst l_
                        | None -> raise (Failure (s^ " not found in scope"))
                            ))
            )
	        | TListRange(s,e) ->
				let start = build_expr s builder scope in
				let endd = build_expr e builder scope in
				L.build_call makeRangeList [| start ; endd |] "range" builder
            | TListLit texlst -> 
                let ty_code = match typ with
                         TconList Int -> 0
                       | TconList Bool -> 1
                       | TconList Char -> 2
                       | TconList Float -> 3
                       | TconList (TconList _) -> 4
                       | TconList (TconTuple _) -> 5
                       | TconList (Tmaybe _) -> 6
                       | ty -> print_endline ("couldnt make list of "^(ty_to_str ty)); -1
                in
                let emptylist = L.build_call makeEmptyList [| L.const_int i32_t ty_code |]
                "empty" builder in

                let rec build_list s prevlist = (match s with
                    | h :: t -> 
                        let tn_star = build_expr h builder scope in
                        let n_star = L.build_call makeNode [| tn_star |] "makeNode" builder in
                        let nextlist = L.build_call appendNodeThunk
                            [| prevlist ; n_star |] "appendNodeThunk" builder in
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
            | TApp(t1, t2) -> let lv1 = build_expr t1 builder scope in
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
            | TAnd -> andb_init_thunk
            | TOr -> orb_init_thunk
            | TNot -> notb_init_thunk
            | TCons -> cons_init_thunk
            | TCat -> cat_init_thunk
            | TLen -> length_init_thunk
            | THead -> head_init_thunk
            | TTail -> tail_init_thunk
            | TFirst -> first_init_thunk
            | TSec -> second_init_thunk
            | TIs_none -> is_none_init_thunk
            | TFrom_just -> from_just_init_thunk
           (* | TFrom_just -> from_just_init_thunk*)
	        | TInt_to_float -> int_to_float_init_thunk
            | TLambda(_, _) -> raise(Failure "unexpected lambda")
            | TTuple(tx1,tx2) ->
                    let (t1,t2) = (match typ with 
                            | (TconTuple l) -> l
                            | _ -> raise(Failure "expected TconTuple")) in
                    let first = build_expr tx1 builder scope in
                    let sec = build_expr tx2 builder scope in
                    L.build_call makeTuple 
                    [| first ; sec 
                    ; L.const_int i32_t (ty_to_int t1) 
                    ; L.const_int i32_t (ty_to_int t2) |] "tup" builder
            | TJust(tx) -> let inn = build_expr tx builder scope in
                    let t = (match typ with
                            | (Tmaybe l) -> l
                            | _ -> raise (Failure "expected Tmaybe")) in
                    L.build_call makeMaybe 
                    [| inn ; L.const_int i32_t (ty_to_int t) |] "just" builder
            | TNone -> 
                    let t = (match typ with
                            | (Tmaybe l) -> l
                            | _ -> raise (Failure "expected Tmaybe")) in
                    L.build_call makeMaybe 
                    [| L.const_null (L.pointer_type struct_thunk_type)
                    ; L.const_int i32_t (ty_to_int t) |] "none" builder

            | TListComp (tex, tclslst) ->
                    let ty_code = match typ with
                                 TconList Int -> 0
                               | TconList Bool -> 1
                               | TconList Char -> 2
                               | TconList Float -> 3
                               | TconList (TconList _) -> 4
                               | TconList (TconTuple _) -> 5
                               | TconList (Tmaybe _) -> 6
                               | ty -> print_endline ("couldnt make list comp of "^(ty_to_str ty)); -1
                    in
                    (*let num_vars = 
                        let rec num_list lst = (match lst with
                            | h::t -> (match h with
                                    | TListVBind _ -> 1 + num_list t
                                    | _ -> num_list t
                            )
                            | [] -> 0
                        )
                    in num_list tclslst in*)

                    let rec get_lists clauses = match clauses with
                        | h::t -> (match h with
                            | TListVBind (str, texp) ->
                                let b = build_expr texp builder scope in
                                    (str, b)::(get_lists t)
                            | _ -> get_lists t
                        )
                        | [] -> []
                    in
                    let rec get_filters clauses = match clauses with
                        | h::t -> (match h with
                            | TFilter texp ->
                                let b = build_expr texp builder scope in
                                    b::(get_filters t)
                            | _ -> get_filters t
                        )
                        | [] -> []
                    in
                    let lists = get_lists tclslst in
                    let filters =  get_filters tclslst
                    in
                    
                    let fn_thunk = build_expr tex builder scope in
                    let map_thunk = L.build_call mapl 
                        [| snd (List.hd lists); fn_thunk; L.const_int i32_t ty_code |] "map_thunk" builder in

                    let rec apply_map_list lsts thunk = match lsts with
                        | [] -> thunk
                        | h::t -> let map_list_thunk = L.build_call map_listl
                                [| thunk; snd (List.hd lsts) ; L.const_int i32_t ty_code |] 
                                "map_list_thunk" builder in
                                 apply_map_list t map_list_thunk
                    in
                    let mapped_thunk = apply_map_list (List.tl lists) map_thunk in 
                    let rec apply_filters fltrs thunk = match fltrs with
                        | [] -> thunk
                        | h::t -> let filter_thunk = L.build_call filterl
                                [| thunk; List.hd fltrs ; L.const_int i32_t ty_code |] "filter_thunk"
                                builder
                                in apply_filters t filter_thunk
                    in
                    apply_filters filters mapped_thunk

            | _ -> raise(Failure "couldn't build texpr")
    in
    
    (* GIVE THIS fn_decls *)
    let build_func_body func_decls = function
        | (_,TypedVdef(name,(txpr,Tarrow(l,r)))) ->
            let fn_decl = (match(StringMap.find_opt name func_decls) with
                | Some (decl,_) -> decl
                | None -> raise (Failure ("No function for decl "^name))) in
            let fn_builder = L.builder_at_end context (L.entry_block fn_decl) in
            let vars = lambda_var_list (txpr,Tarrow(l,r)) in
            let argsll = L.params fn_decl in
            let argslll = Array.to_list argsll in
            let var_to_argsll_map = List.fold_left2 (fun map var ll -> 
                StringMap.add var ll map) StringMap.empty vars argslll in
            let var_to_local_map =
                StringMap.mapi (stack_alloc fn_builder) var_to_argsll_map in
            let lbody = throw_away_lambda (txpr, Tarrow(l,r)) in
            let l_body_expr = build_expr lbody fn_builder var_to_local_map in
            let bc = L.build_call invoke [| l_body_expr |] "da" fn_builder in
            add_terminal fn_builder (L.build_ret bc)
        | _ -> raise(Failure "expected tarrow vdef O_o")
    in
    
    let print_expr (lv: L.llvalue) (vtype: ty) =
        (* call invoke on thunk *)
        let _ = L.build_call invoke [| lv |] "invoke" builder in
        (* print *)
        let _ = (match vtype with
            | TconList(t) -> L.build_call printAnyThunk 
				[| lv ; L.const_int i32_t 4|] "" builder
            | Int -> L.build_call printAnyThunk 
				[| lv ; L.const_int i32_t 0 |] "" builder
            | Bool -> L.build_call printAnyThunk 
				[| lv ; L.const_int i32_t 1 |] "" builder
            | Float -> L.build_call printAnyThunk 
				[| lv ; L.const_int i32_t 3 |] "" builder
            | Char -> L.build_call printAnyThunk 
				[| lv ; L.const_int i32_t 2 |] "" builder
            | TconTuple _ -> L.build_call printAnyThunk
				[| lv ; L.const_int i32_t 5 |] "" builder
            | Tmaybe _ -> L.build_call printAnyThunk
				[| lv ; L.const_int i32_t 6 |] "" builder
            | ty -> raise(Failure("Main is of unprintable type"^(ty_to_str ty)))
        ) in
        L.build_call printf_func [| char_format_str ; L.const_int i8_t (Char.code('\n')) |] "printf" builder


    in

    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef("main",texp)) ->
                (* build expr *)
                let v = build_expr texp builder StringMap.empty in
                (* print expr if main*)
                ignore (print_expr v (snd texp))
			| (_, TypedVdef(name,(tex,Tarrow(_)))) as tup->
                let _ = build_eval_func_body eval_decls tup in
				build_func_body fn_decls tup
				(* build_func_body *)
            | (_, TypedVdef(name, _)) -> raise(Failure (name ^ " is not an arrow type!"))
    in
    let _ = List.iter build_decl decl_lst in
        ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module
