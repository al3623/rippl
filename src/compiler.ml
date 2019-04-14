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

let print_decls d = match d with
        | Vdef(n, e) ->
                print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e);
        | _ -> print_endline "annots"

let lift_decl curr_list d = match d with
        | Vdef(n, e) -> (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str e);*)
                let (_, nl_ast) = find_lambdas false e in ();
                (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str nl_ast);*)
                (*let _ = Lift_lambdas.print_map() in ();*)
                let mang_ast = mangle_lets nl_ast in
                (*print_endline (n ^ " = " ^ Pretty_type_print.ast_to_str mang_ast);
                print_endline "-------------";*)
                let (lifted, l_decs) = lift mang_ast [] in
                curr_list @ (l_decs @ [Vdef(n, lifted)])



        | _ -> curr_list @ [d]


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
        let program_ll = List.fold_left lift_decl [] m_program in
        List.iter print_decls program_ll;