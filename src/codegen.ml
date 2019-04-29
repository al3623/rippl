module L = Llvm
open Ast
open Tast
open Lib
open Structs
open Thunk
open Mymap

module StringMap = Map.Make(String)

let translate (decl_lst: (decl * typed_decl) list) =

    (* I. GLOBAL MAPS OF VDEFS - LAMBDAS AND NON-LAMBDAS *)
    
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
    (* global map of variables: map var_name to llvalue - initialized to zero values 
     * non-lambdas *)
    let global_vars: L.llvalue StringMap.t = 
        let global_var m decl =
            let vdef = snd decl in
            let (name, zero) = match vdef with
                | TypedVdef(n, _) -> (n, L.const_int i32_t 0) in
            StringMap.add name (L.define_global name zero the_module) m in
        List.fold_left global_var StringMap.empty var_lst in

    (* TEST: print the global vars (not lambdas) *)
    print_endline "* * * Non-Lambda Global variables: * * *";
    StringMap.iter (fun k v -> print_endline k) global_vars;

    (* fn to get number of arguments of lambda *)
    let rec num_args lm = match lm with
        | TLambda(_,(tex, _)) -> 1 + num_args tex
        | _ -> 0
    in
    (* convert Tlambda -> Tlambda_def *)
    let tldef_convert (tlambda: tx) (name: string) = match tlambda with
        | TLambda(var, e) ->
            { tlname = name
				; tltyp = Tvar var
				; trtyp = snd e
				; tlexp = (TVar var, Tvar var)
				; trexp = e }
        | _ -> raise (Failure "not Tlambda")
    
    in

    (* list of tlambda_def's *)
    let lm_defs: (tlambda_def list) =
        let to_lmdef (dec: decl * typed_decl) = match dec with
            | (_,TypedVdef(n,(l,_))) -> tldef_convert l n
        in
    List.map to_lmdef lambda_lst
    in

    (* get array of arg types from lambda def *)
    let rec lm_arg_types (ldecl: tlambda_def) =


    in
    
    (* get return type of lambda *)
    let rec lm_ret_type (ldecl: tlambda_def) =


    in

    (* map of tlambda_def's *)
    let lm_decls: (L.llvalue * tlambda_def) StringMap.t =
        let lambda_decl m ldecl =
            let name = ldecl.tlname in
            and arg_types = lm_arg_types ldecl in
        let ftype = L.function_type (lm_ret_type ldecl arg_types in
        StringMap.add name (L.define_function name ftype the_module, ldecl) m in
    List.fold_left lambda_decl StringMap.empty lambda_lst in


    (* II. BUILD FUNCTION (LAMBDA) BODY *)
    let rec build_lambda (lm_decl: tlambda_def) = 
        let (the_function, _) = StringMap.find lm_decl.tlname lambda_decls in
        let builder = L.builder_at_end context (L.entry_block the_function)
    
        
        
        
    in

    (* III. BUILD EXPRESSION *)
    let rec build_expr builder (txp: typed_expr) = match txp with
        | _ -> raise(Failure "build_expr: Not Implemented")
    
    in

    (* IV. PRINT EXPRESSION *)

    let print_expr (texp: typed_expr) = match texp with
        | _ -> raise(Failure "print_expr: Not implemented")
    in

    (* V. BUILD ALL *)
    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef(name,texp)) -> 
                (* build expr *)
                let _ = build_expr builder (fst texp) in
                (* print expr if main*)
                if name = "main" then ignore (print_expr texp) else ()
    in
    let _ = List.iter build_decl decl_lst in
    ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module

