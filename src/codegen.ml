module L = Llvm
open Ast
open Tast
open Lib

module StringMap = Map.Make(String)

let translate (decl_lst: (decl * typed_decl) list) =
    
    (* function to print typed expressions *)
    let print_texpr (tlit: tx) (lval: L.llvalue) =
        let tycode = match tlit with
            | TBoolLit _ -> L.const_int i32_t 1
            (* tycode unused for all other tx *)
            | _ -> L.const_int i32_t (-1)
        in
        match tlit with
            | TIntLit n ->
                let _ = L.build_call printf_func [| int_format_str ; L.const_int i32_t n |] "printf" builder in
                    L.build_call printf_func [| char_format_str ; l_char |] "printf" builder
            | TBoolLit _ -> 
                let b = L.build_call makeBool [| lval |] "makeBool" builder in  
                    let _ = L.build_call printAny [| b ; tycode |] "" builder in
                    L.build_call printf_func [| char_format_str ; l_char |] "printf" builder
            | TCharLit c -> 
                let _ = L.build_call printf_func [| char_format_str ; L.const_int i8_t (Char.code c) |] "printf" builder in
                    L.build_call printf_func [| char_format_str ; l_char |] "printf" builder
            | TFloatLit f -> 
                let _ =  L.build_call printf_func [| float_format_str ; L.const_float float_t f |] "printf" builder in
                    L.build_call printf_func [| char_format_str ; l_char |] "printf" builder
            | _ -> raise (Failure "Printing not implemented")
    in

    (* build code for typed_expr *)
    let rec build_expr builder ((tx, _): typed_expr) = 
        match tx with
        | TIntLit n -> L.const_int i32_t n
        | TBoolLit b -> L.const_int i1_t (if b then 1 else 0)
        | TCharLit c -> L.const_int i8_t (Char.code c)
        | TFloatLit f -> L.const_float float_t f
        | TIte(cond, thn, els) -> raise (Failure "ite codegen") 

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
    
    (* print map keys *) 
    let print_vars key v = print_endline ("var: " ^ key) in
        StringMap.iter print_vars global_vars;

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
                    | TIntLit _ as i   -> print_texpr i e
                    | TBoolLit _ as b  -> print_texpr b e
                    | TCharLit _ as c  -> print_texpr c e
                    | TFloatLit _ as f -> print_texpr f e
                    | _ -> raise (Failure "print texpr")
                )
    in
    let _ = List.iter build_decl decl_lst in
    (* return 0 *)
    ignore (L.build_ret (L.const_int i32_t 0) builder);
    the_module

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(*
    let translate decl_lst =

        let rec texpr_ptr texpr = match texpr with
                | (tx, _) -> match tx with 
                        | TIntLit n -> let int_ = L.const_int i32_t n in
                        let intstar = L.build_call makeInt [| int_ |] "makeInt" builder in 
                        L.build_call makeIntVoid [| intstar |] "makeIntVoid" builder
                        | TFloatLit f -> let float_ = L.const_float float_t f in
                        let floatstar = L.build_call makeFloat [| float_ |] "makeFloat" builder in
                        L.build_call makeFloatVoid [| floatstar |] "makeFloatVoid" builder
                        | TBoolLit b -> let bool_ = L.const_int i1_t (if b then 1 else 0) in
                        L.build_call makeBool [| bool_ |] "makeBool" builder
                        | TCharLit c -> let char_ = L.const_int i8_t (Char.code c) in
                        L.build_call makeChar [| char_ |] "makeChar" builder
                        | _ -> raise (Failure "tx not implemented")
                        in

        (* find and execute/print main *)
        let print_main vd = match vd with
                | (_, (TypedVdef ("main", (TListLit(clist), TconList(Char))))) ->
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
                                        L.build_call printPrimList [| prevlist |]
                                        "printPrimList" builder ;
                                        L.build_call printf_func [| char_format_str ; l_char |]
                                        "printf" builder) 
                                        in
                        let _ = print_string clist emptylist in
                        L.build_ret (L.const_int i32_t 0) builder;

                                | (_, (TypedVdef ("main", (TListRange(start_end), TconList(Int))))) ->
                                        let rangelist = makerangelist start_end "mainlist" in
                                        let _ = L.build_call printRangeList [| rangelist |]
                                        "printRangeList" builder in
                                        let _ =	L.build_call printf_func [| char_format_str ; l_char |]
                                        "printf" builder in
                                        L.build_ret (L.const_int i32_t 0) builder
                                | (_, (TypedVdef ("main", (TListLit([]), TconList(_))))) ->
                                        let emptylist = L.build_call makeEmptyList [| L.const_int i32_t 0 |]
                                        "emptyl" builder in
                                        let _ = L.build_call printPrimList [| emptylist |]
                                        "printPrimList" builder in
                                        let _ =	L.build_call printf_func [| char_format_str ; l_char |]
                                        "printf" builder in
                                        L.build_ret (L.const_int i32_t 0) builder
                                | (_, (TypedVdef ("main", (TIte(cond, then_expr, else_expr), typ)))) ->
                                        (* evaluate condition *)
                                        let condition = match cond with
                                | (TBoolLit b, _) -> b
                                | _ -> raise(Failure "not boolean") in
                                        let expr_ptr = if condition then texpr_ptr then_expr else texpr_ptr else_expr in
                                        let ty_code = match typ with
                                | Int -> L.const_int i32_t 0
                                | Bool -> L.const_int i32_t 1
                                | Char -> L.const_int i32_t 2
                                | Float -> L.const_int i32_t 3
                                | _ -> raise (Failure "type")
                                        in
                        let _ = L.build_call printAny [| expr_ptr ; ty_code |] "" builder in
                        (* print newline *)
                        let _ = L.build_call printf_func [| char_format_str ; l_char |]
                        "printf" builder in 
                        (* return 0 *)
                        L.build_ret (L.const_int i32_t 0) builder

                (* codegen for lambdas 
                 * ex: fun x -> x   2
                 *) 
                                | (_, (TypedVdef("main", (TLambda(texpr1, texpr2), typ)))) -> raise(Failure ".")
                                | _ -> raise (Failure "NO")
                        in 
    let _ = print_main (List.hd decl_lst) in
    the_module
*)

