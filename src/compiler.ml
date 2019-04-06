open Ast
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

let print_decl d = 
        match d with
        | Vdef(n, e) -> (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e);*)
                let _ = find_lambdas false e in ()
        | _ -> print_endline "annot"

let _ =
        let lexbuf = Lexing.from_channel stdin in
        let program = Parser.program Scanner.token lexbuf in
        let m_program = Lift_lambdas.transform_main program in
        (*let m = Codegen.translate program in
        Llvm_analysis.assert_valid_module m;
        let ls = Llvm.string_of_llmodule m in*)
        (*let file = "hello.rply" in
        let oc = open_out file in
        fprintf oc "%s\n" ls; 
        close_out oc;
        Sys.command ("cat " ^ file ^ " | lli")*)
        List.iter print_decl m_program;