module L = Llvm
open Ast
open Tast
open Lib

module StringMap = Map.Make(String)

let translate (decl_lst: (decl * typed_decl) list) =

    let print_newline () =  L.build_call printf_func [| char_format_str ; l_char |] "printf" builder in
    
    (* function to print typed expressions *)
    let print_texpr (tlit: tx) (lval: L.llvalue) =
        match tlit with
            | TIntLit n ->
                let _ = L.build_call printf_func [| int_format_str ; L.const_int i32_t n |] "printf" builder in
                    print_newline ()
            | TBoolLit _ -> 
                let b = L.build_call makeBool [| lval |] "makeBool" builder in  
                    let _ = L.build_call printAny [| b ; L.const_int i32_t 1 |] "" builder in
                    print_newline ()
            | TCharLit c -> 
                let _ = L.build_call printf_func [| char_format_str ; L.const_int i8_t (Char.code c) |] "printf" builder in
                    print_newline ()
            | TFloatLit f -> 
                let _ =  L.build_call printf_func [| float_format_str ; L.const_float float_t f |] "printf" builder in
                    print_newline ()
            
            | _ -> raise (Failure "print_texpr") 
    in

    (* build code for typed_expr *)
    let rec build_expr builder (t: typed_expr) = 
        match t with
        (* literals *)
        | (TIntLit n, _) -> L.const_int i32_t n
        | (TBoolLit b, _) -> L.const_int i1_t (if b then 1 else 0)
        | (TCharLit c, _) -> L.const_int i8_t (Char.code c)
        | (TFloatLit f, _) -> L.const_float float_t f
        
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
                    print_newline ()
            )
            in
            print_string clist emptylist

        (* list ranges *)
        | (TListRange(start_end), TconList(Int)) ->
            let rangelist = makerangelist start_end "mainlist" in
            let _ = L.build_call printRangeList [| rangelist |] "printRangeList" builder in
            print_newline ()

        (* primitive lists *)
        | (TListLit([]), TconList(_)) ->
            let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 0 |] "emptyl" builder in
            let _ = L.build_call printPrimList [| emptylist |] "printPrimList" builder in
            print_newline ()

        | _ -> raise (Failure "codegen for expr not implemented")
    in

    (* d_lst is (name, expr) list representing (annot, Vdef) list *)
    let d_lst : (string * typed_expr) list = 
        let tvdef_tup = function
            (_,TypedVdef(s, e)) -> (s,e) in
        List.map tvdef_tup decl_lst in

    (* global map of Vdefs: map var_name to llvalue, initialized to zero *)
    let global_vars: L.llvalue StringMap.t = 
        let global_var m (var, e) =
            let llval = L.const_int i8_t 0 in
            StringMap.add var (L.define_global var llval the_module) m in
        List.fold_left global_var StringMap.empty d_lst in
    
    (* tdecl is (decl = annotation, typed_decl = TypedVdef(string, typed_expr))
     * build_decl builds the code for a TypedVdef
     *)
    let rec build_decl (tdecl: (decl * typed_decl)) =
        match tdecl with
            | (_, TypedVdef(_,texp)) -> 
                let txp = fst texp in
                (* build expr *)
                let e = build_expr builder texp in
                ignore (
                (* print expr *)
                match txp with
                    | TIntLit   _ as i -> print_texpr i e
                    | TBoolLit  _ as b -> print_texpr b e
                    | TCharLit  _ as c -> print_texpr c e
                    | TFloatLit _ as f -> print_texpr f e
                    
                    | _ -> L.const_int i1_t 0
                )
    in
    let _ = List.iter build_decl decl_lst in
    (* return 0 *)
    ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module

