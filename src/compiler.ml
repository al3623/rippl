open Ast
open Tast
open Scanner
open Parser
open Check_main
open Pair_annots
open Pretty_type_print
(*open Type_check*)
open Lift_lambdas
open Check_lists
open Codegen
open Printf
open Type_inference
open Remove_substs
(*open Pretty_tast_print*)

let print_decl d = 
        match d with
        | Vdef(n, e) -> print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e);
                (*let (_, e_named_lambdas) = find_lambdas false e in ();*)
                (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e_named_lambdas);*)
                (*let _ =Lift_lambdas.print_map() in ()*)

        | _ -> print_endline "annot"

let print_tdecl td = 
    match td with
    | (annot, tvd) -> print_endline ( Pretty_tast_print.tast_to_str tvd)


let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let pair_program = Pair_annots.pair_av program in
        let pair_iprogram = Type_inference.type_paired_program pair_program in
		let pair_tprogram = Remove_substs.remove_subst_pairs pair_iprogram in
        (*let m = Codegen.translate program in
        Llvm_analysis.assert_valid_module m;
        let ls = Llvm.string_of_llmodule m in*)
        (*let file = "hello.rply" in
        let oc = open_out file in
        fprintf oc "%s\n" ls; 
        close_out oc;
        Sys.command ("cat " ^ file ^ " | lli")
        List.iter print_tdecl pair_tprogram; *)
		()
