module L = Llvm
open Ast
open Tast
open Lib
open Structs
open Thunk
open Mymap

module StringMap = Map.Make(String)

let translate (decl_lst: (decl * typed_decl) list) =

    (* lltype of type *)
    let ltype_of_typ = function
          Int -> i32_t
        | Bool -> i1_t
        | Char -> i8_t
        | Float -> float_t
        | _ -> (* TODO: more types *) raise (Failure "ltype_of_typ")
    in

    (* convert lambda arg to lltype *)
    let array_of_arg (t: typed_expr) = match t with
        | (texp, ty) -> [| ltype_of_typ ty |]
    in
    
    (* convert Tlambda -> Tlambda_def *)
	(* TODO: Fix this *) 
    let tldef_convert (tlambda: tx) (name: string) = match tlambda with
        | TLambda(var, e2) ->  
            { tlname = name
				; tltyp = (*snd e1*) Tvar "A"
				; trtyp = snd e2
				; tlexp = (TVar(var), Tvar "A")
				; trexp = e2 }
        | _ -> raise (Failure "not Tlambda")
    
    in

    (* list of tlambda_def's *)
    let lambdas: (tlambda_def list) =
        (* traverse Vdefs for lambdas *)
        let rec get_lambdas d_lst = (match d_lst with
            | (_, tdecl)::t -> (match tdecl with
                TypedVdef(name,(texp, _)) -> 
                    (match texp with
                        | TLambda(_, _) as tl -> (tldef_convert tl name) :: (get_lambdas t)
                        | _ -> get_lambdas t
                    )
            )
            | [] -> []
        )
    in get_lambdas decl_lst in
    
    (* global map of lambda definitions *)
    let lambda_decls : (L.llvalue * tlambda_def) StringMap.t = 
        let lambda_decl m tldef =
            let name = tldef.tlname
            and arg_array = array_of_arg tldef.tlexp in
        let ltype = L.function_type (ltype_of_typ tldef.trtyp) (array_of_arg tldef.tlexp) in
        StringMap.add name (L.define_function name ltype the_module, tldef) m in
    List.fold_left lambda_decl StringMap.empty lambdas in

    (* code for printing newline *)    
    let print_blankline () =  L.build_call printf_func [| char_format_str ; l_char |] "printf" builder in
    
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

    (* d_lst is (name, expr) list representing (annot, Vdef) list *)
    let d_lst : (string * typed_expr) list = 
        let tvdef_tup = function
            (_,TypedVdef(s, e)) -> (s,e) in
        List.map tvdef_tup decl_lst in

    (* global map of Vdefs: map var_name to llvalue *)
    let global_vars: L.llvalue StringMap.t = 
        let global_var m (var, e) =
            let llval = build_expr builder (fst e) in
            StringMap.add var (L.define_global var llval the_module) m in
        List.fold_left global_var StringMap.empty d_lst in
    
    (* tdecl is (decl = annotation, typed_decl = TypedVdef(string, typed_expr))
     * build_decl builds the code for a TypedVdef
     *)
    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef(name,texp)) -> 
                (* build expr *)
                let _ = build_expr builder (fst texp) in
                (* print expr *)
                if name = "main" then ignore (print_texpr texp) else ()
    in
    let _ = List.iter build_decl decl_lst in
    ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module

