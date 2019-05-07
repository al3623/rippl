module L = Llvm
open Ast
open Tast
open Lib
open Structs
open Thunk
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
        
        | _ -> raise(Failure "ltyp_of_typ")
    
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
            StringMap.add eval_name (L.define_function eval_name 
                eval_func_type the_module, lm_def) m
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
            let ftype = L.function_type (L.pointer_type i8_t) fn_args in
            StringMap.add fname (L.define_function fname ftype the_module,
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
    (*
    let thunks: L.llvalue StringMap.t =
        let build_thunk m (lmd: tlambda_def) =
            (* get number of args of function *)
            let rec argnum texp = match texp with
                  (TLambda(_,t),_) -> 1 + (argnum t)
                | _ -> 0
            in
            let argc = 1 + (argnum lmd.trexp) in
            let eval_name = "$eval_" ^ lmd.tlname in
            let eval_fn = fst (StringMap.find eval_name eval_decls) in
            let num_args = L.const_int i32_t argc in
            let thunk_name = "$$" ^ lmd.tlname ^ "_thunk" in

            let thunk_lval = L.build_call initThunk [| eval_fn; num_args |] 
                thunk_name builder in
            
            (* TEST *)
            let _ = print_endline (lmd.tlname ^ " thunk: " ^ (string_of_int 
                argc) ^ " args") in

            StringMap.add thunk_name thunk_lval m
        in
        List.fold_left build_thunk StringMap.empty lm_defs
    *)
    
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
                L.build_call makeBool [| L.const_int i1_t x |] "bool" builder

            | _ -> raise (Failure "make_ptr")
        in

        let tex = fst texp in match tex with
            (* literals - build thunk literals *)
              TIntLit _ -> let n = make_ptr tex in L.build_call 
                initThunkLiteral [| n |] "int_lit_thunk" builder
            | TFloatLit _ -> let f = make_ptr tex in L.build_call 
                initThunkLiteral [| f |] "float_lit_thunk" builder
            | TCharLit _ -> let c = make_ptr tex in L.build_call 
                initThunkLiteral [| c |] "char_lit_thunk" builder
            | TBoolLit _ -> let b = make_ptr tex in L.build_call
                initThunkLiteral [| b |] "bool_lit_thunk" builder
            | TVar s -> (match (StringMap.find_opt s scope) with
                  Some lval -> lval
                | None -> (match (StringMap.find_opt s global_vars) with
                      Some l -> l
                    | None -> (match (StringMap.find_opt s fn_decls) with
                          Some l_ -> fst l_
                        | None -> raise (Failure (s^ " not found in scope"))
                            ))
            )
            | TLet (ta, t) -> (match ta with
                                  TAssign (s, te) -> let v1 = build_expr te builder scope in
                                        let new_scope = StringMap.add s v1 scope in
                                        build_expr t builder new_scope
            )
            (* Application *)
            | TApp(t1, t2) as tapp -> let lv1 = build_expr t1 builder scope in
                let lv2 = build_expr t2 builder scope in
                L.build_call apply [| lv1; lv2 |] "apply" builder
            
(*
                (* get args *)
                let rec get_args t = match t with 
                      TApp((x,_),(arg,_)) -> 
                          let llptr = make_ptr arg in llptr::(get_args x)
                    | _ -> []
                in
                let args = List.rev (get_args tapp) in
                (* get name of fn *)
                let rec get_fname t = match t with
                      TApp(x,_) -> (match x with
                          TApp(y,_) -> get_fname y
                        | TVar s -> s
                        | _ -> raise (Failure "get_fname"))
                    | _ -> raise (Failure "not app")
                in
                let fname = get_fname tapp in

                let initial_thunk = StringMap.find fname thunks in

                (* call apply on each arg *)
                let rec apply_arg thunk arglst = match arglst with
                      h::t -> let thk = L.build_call apply [| thunk; h |] 
                        "apply" builder in apply_arg thk t
                    | [] -> thunk
                in
                apply_arg initial_thunk args

            | TApp(e, b) ->  (
                let u = match e with
                      (TApp(_,_),_) -> true
                    | _             -> false
                in 
                if u then (match e with
                      (TNeg,_) -> 
                        let b' = build_expr builder (fst b) in
                        L.build_neg b' "neg" builder
                    | (TNot,_) ->
                        let b' = build_expr builder (fst b) in
                        L.build_not b' "not" builder
                    | (TVar s, _) -> 
                    | _ -> raise (Failure "TApp")
                )
                else
                let a = match e with
                    | (TApp(_,x),_) -> x
                    | _ -> raise (Failure "not app")
                in
                let a' = build_expr builder (fst a) in
                let b' = build_expr builder (fst b) in
                (match e with
                    | (TApp((op,_),_),_) -> (match op with

                          TAdd      -> L.build_add
                        | TSub      -> L.build_sub
                        | TMult     -> L.build_mul
                        | TDiv      -> L.build_sdiv
                        | TMod      -> L.build_srem

                        | TPow      -> (* TODO *) L.build_add
                        
                        | TAddF     -> L.build_fadd
                        | TSubF     -> L.build_fsub
                        | TMultF    -> L.build_fmul
                        | TDivF     -> L.build_fdiv
                        
                        | TPowF     -> (* TODO *) L.build_fadd
                        
                        | TEq       -> L.build_icmp L.Icmp.Eq
                        | TEqF      -> L.build_fcmp L.Fcmp.Oeq
                        | TNeq      -> L.build_icmp L.Icmp.Ne
                        | TNeqF     -> L.build_fcmp L.Fcmp.One
                        | TGeq      -> L.build_icmp L.Icmp.Sge
                        | TGeqF     -> L.build_fcmp L.Fcmp.Oge
                        | TLeq      -> L.build_icmp L.Icmp.Sle
                        | TLeqF     -> L.build_fcmp L.Fcmp.Ole
                        | TLess     -> L.build_icmp L.Icmp.Slt
                        | TLessF    -> L.build_fcmp L.Fcmp.Olt
                        | TGreater  -> L.build_icmp L.Icmp.Sgt
                        | TGreaterF -> L.build_fcmp L.Fcmp.Ogt
                        
                        | TAnd      -> L.build_and
                        | TOr       -> L.build_or
                        
                        | _         -> raise (Failure "binop")
                        )
                    | _ -> raise (Failure "not TApp")
                ) a' b' "tmp" builder
            )

            | _ -> raise (Failure "build_expr: not implemented")
 *)
    in
    (* build eval function body *)
    let build_evalfn_body (lm_def: tlambda_def) =
        let (eval_fn, _) = StringMap.find ("$eval_"^ lm_def.tlname) 
                eval_decls in
        let builder = L.builder_at_end context (L.entry_block eval_fn) in
        
        (* get arg values from thunk struct *)

        (* call the core function *)

        (* return the result *)

        let fn_builder = builder in
        add_terminal fn_builder (L.build_ret (L.const_pointer_null 
                (L.pointer_type i8_t)))
    in
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

        (* get final expression in function *)
        let e = 
            let rec fin_ex = function
              TLambda(_,(t,_)) -> fin_ex t
            | x -> x
        in fin_ex (fst lm_def.trexp) 
        in

        let fn_builder = builder in
        add_terminal fn_builder (L.build_ret (L.const_pointer_null 
                (L.pointer_type i8_t)))
    in
    
    let print_expr (texp: typed_expr) = match texp with
        | _ -> raise(Failure "print_expr: Not implemented")
    in



(*    
    (* build non-function Vdefs *)
    let build_tdecl (tdecl: (decl * typed_decl)) builder =
        match tdecl with
            (_,TypedVdef(n,tex)) -> build_expr tex builder
    in 
    
    (* build function bodies *)
    let _ = List.iter build_evalfn_body lm_defs in
    let _ = List.iter build_fn_body lm_defs in
    
    (* build non-function Vdefs *)
    let _ = List.iter build_tdecl var_lst in

*)

(*
    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef(name,texp)) -> 
                (* build expr *)
                let _ = build_expr builder (fst texp) in
                (* print expr if main*)
                if name = "main" then ignore (print_expr texp) else ()
    in
    let _ = List.iter build_decl decl_lst in
*)  
    ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module


(*
(* function to print typed expressions *)
    let print_texpr (t: typed_expr) = 
        match t with
            | (TIntLit n, _) ->
                let _ = L.build_call printf_func [| int_format_str ; L.const_int i32_t n |] "printf" builder in
                    print_blankline ()
            | (TBoolLit b, _) ->
                let bool_ = (if b = true then L.const_int i8_t 1 else L.const_int i8_t 0) in
                let _ = L.build_call printBool [| bool_ |] "" builder in
                    print_blankline ()
            | (TCharLit c, _) -> 
                let _ = L.build_call printf_func [| char_format_str ; L.const_int i8_t (Char.code c) |] "printf" builder in
                    print_blankline ()
            | (TFloatLit f, _) -> 
                let _ =  L.build_call printf_func [| float_format_str ; L.const_float float_t f |] "printf" builder in
                    print_blankline ()
         
            (* strings = char lists*)
            | (TListLit(clist), TconList(Char)) ->
                let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 2 |]
                "empty" builder in
                let rec print_string str prevlist = (match str with
                    | (TCharLit(c),char_ty) :: xs -> 
                        let l_char = L.const_int i8_t (Char.code c) in
                        let charstar = L.build_call makeChar
                            [| l_char |] "makeChar" builder in
                        let nodestar = L.build_call makeNode
                            [| charstar |] "makeNode" builder in
                        let nextlist = L.build_call appendNode
                            [| emptylist ; nodestar |] "appendNode" builder in
                        print_string xs nextlist
                    | _ ->
                        let _ = L.build_call printPrimList [| prevlist |] "printPrimList" builder in
                        print_blankline ()
                )
                in
                print_string clist emptylist

            (* list ranges *)
            | (TListRange(start_end), TconList(Int)) ->
                let rangelist = makerangelist start_end "mainlist" in
                let _ = L.build_call printRangeList [| rangelist |] "printRangeList" builder in
                print_blankline ()

            (* primitive lists *)
            | (TListLit([]), TconList(_)) ->
                let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 0 |] "emptyl" builder in
                let _ = L.build_call printPrimList [| emptylist |] "printPrimList" builder in
                print_blankline ()
    
            | _ -> raise (Failure "print_texpr") 
    in

    (* build code for typed_expr *)
    let rec build_expr builder (t: tx) = 
        match t with
        (* literals *)
        | TIntLit n -> L.const_int i32_t n
        | TBoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | TCharLit c -> L.const_int i8_t (Char.code c)
        | TFloatLit f -> L.const_float float_t f 
        (* application *)
        | TApp(e, b) ->  (
            let u = match e with
                  (TApp(_,_),_) -> "binop"
                | _             -> "unop"
            in 
            if u = "unop" then (match e with
                  (TNeg,_) -> 
                    let b' = build_expr builder (fst b) in
                    L.build_neg b' "neg" builder
                | (TNot,_) ->
                    let b' = build_expr builder (fst b) in
                    L.build_not b' "not" builder
                | _ -> raise (Failure "unop")
            )
            else
            let a = match e with
                | (TApp(_,x),_) -> x
                | _ -> raise (Failure "not app")
            in
            let a' = build_expr builder (fst a) in
            let b' = build_expr builder (fst b) in
            (match e with
                | (TApp((op,_),_),_) -> (match op with

                      TAdd      -> L.build_add
                    | TSub      -> L.build_sub
                    | TMult     -> L.build_mul
                    | TDiv      -> L.build_sdiv
                    | TMod      -> L.build_srem

                    | TPow      -> (* TODO *) L.build_add
                    
                    | TAddF     -> L.build_fadd
                    | TSubF     -> L.build_fsub
                    | TMultF    -> L.build_fmul
                    | TDivF     -> L.build_fdiv
                    
                    | TPowF     -> (* TODO *) L.build_fadd
                    
                    | TEq       -> L.build_icmp L.Icmp.Eq
                    | TEqF      -> L.build_fcmp L.Fcmp.Oeq
                    | TNeq      -> L.build_icmp L.Icmp.Ne
                    | TNeqF     -> L.build_fcmp L.Fcmp.One
                    | TGeq      -> L.build_icmp L.Icmp.Sge
                    | TGeqF     -> L.build_fcmp L.Fcmp.Oge
                    | TLeq      -> L.build_icmp L.Icmp.Sle
                    | TLeqF     -> L.build_fcmp L.Fcmp.Ole
                    | TLess     -> L.build_icmp L.Icmp.Slt
                    | TLessF    -> L.build_fcmp L.Fcmp.Olt
                    | TGreater  -> L.build_icmp L.Icmp.Sgt
                    | TGreaterF -> L.build_fcmp L.Fcmp.Ogt
                    
                    | TAnd      -> L.build_and
                    | TOr       -> L.build_or
                    
                    | _         -> raise (Failure "binop")
                    )
                | _ -> raise (Failure "not TApp")
            ) a' b' "tmp" builder
        )
        (* lambdas *)
        | TLambda (x, f_x) -> (* TODO *) L.const_int i1_t 0

        | _ -> (* TODO: code for other typed_expr *) L.const_int i1_t 0 
    in
*) 
