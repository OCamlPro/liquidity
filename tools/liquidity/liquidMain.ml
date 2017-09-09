(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: We could use a simplify pass to propagate the no-effect
  defining expression of once-used variables to their uniq use
  site. It could dramatically decrease the size of the stack.  *)


open Tezos_context
open LiquidTypes

(* We use the parser of the OCaml compiler parser to parse the file,
  we then translate it to a simplified AST, before compiling it
  to Michelson. No type-checking yet.
 *)

let arg_peephole = ref true
let arg_keepon = ref false

let compile_liquid_file filename =
  let ocaml_ast = LiquidFromOCaml.read_file filename in
  let syntax_ast, env = LiquidFromOCaml.translate filename ocaml_ast in
  FileString.write_file (filename ^ ".syntax")
                        (LiquidPrinter.Liquid.string_of_contract
                           syntax_ast);
  let typed_ast, to_inline = LiquidCheck.types env syntax_ast in
  FileString.write_file (filename ^ ".typed")
                        (LiquidPrinter.Liquid.string_of_contract
                           typed_ast);

  let live_ast = LiquidSimplify.simplify_contract typed_ast to_inline in
  FileString.write_file (filename ^ ".simple")
                        (LiquidPrinter.Liquid.string_of_contract
                           live_ast);

  let pre_michelson = LiquidMichelson.translate live_ast in

  let pre_michelson =
    if !arg_peephole then
      LiquidPeephole.simplify pre_michelson
    else
      pre_michelson
  in
  let michelson_ast = LiquidEmit.emit_contract pre_michelson in

  let output = filename ^ ".tz" in
  FileString.write_file output
                        (try
                           LiquidPrinter.Michelson.string_of_contract
                             michelson_ast
                         with
                         | Error ->
                            LiquidToTezos.string_of_contract
                              (LiquidToTezos.convert_contract pre_michelson));
  Printf.eprintf "File %S generated\n%!" output;
  Printf.eprintf "Typecheck with:\n  tezos-client typecheck program %s\n" output


let read_tezos_file filename =
  let s = FileString.read_file filename in
  let contract_hash = Hash.Operation_hash.hash_bytes [s] in
  match LiquidFromTezos.contract_of_string s with
  | Some code ->
     Printf.eprintf "Program %S parsed\n%!" filename;
     code, contract_hash
  | None ->
     Printf.eprintf "Errors parsing in %S\n%!" filename;
     exit 2

let compile_tezos_file filename =
  let code, contract_hash = read_tezos_file filename in

  let c = LiquidFromTezos.convert_contract code in
  let c = LiquidClean.clean_contract c in
  let c = LiquidInterp.interp c in
  begin
    FileString.write_file  (filename ^ ".dot")
                           (LiquidDot.to_string c);
    let cmd = Ocamldot.dot2pdf_cmd (filename ^ ".dot") (filename ^ ".pdf") in
    if Sys.command cmd <> 0 then
      Printf.eprintf "Warning: could not generate pdf from .dot file\n%!";
  end;
  let c = LiquidDecomp.decompile c in
  FileString.write_file  (filename ^ ".liq.pre")
                         (LiquidPrinter.Liquid.string_of_contract c);
  let env = LiquidFromOCaml.initial_env filename in
  let typed_ast, to_inline = LiquidCheck.types env c in
  Printf.eprintf "Inlining: %d\n%!" (StringMap.cardinal to_inline);
  let live_ast = LiquidSimplify.simplify_contract typed_ast to_inline in
  let untyped_ast = LiquidUntype.untype_contract live_ast in
  let output = filename ^ ".liq" in
  FileString.write_file  output
                         (try
                            LiquidToOCaml.string_of_structure
                              (LiquidToOCaml.structure_of_contract
                                 untyped_ast)
                          with Error ->
                            LiquidPrinter.Liquid.string_of_contract
                              untyped_ast);
  Printf.eprintf "File %S generated\n%!" output;
  ()

let contract_amount = ref "1000.00"
let contract_arg = ref (Script_repr.Prim(0, "Unit", []))
let contract_storage = ref (Script_repr.Prim(0, "Unit", []))

let context = ref None

let get_context () =
  match !context with
  | Some ctxt -> ctxt
  | None ->
     let (level : int32) = 1l in
     let (timestamp : int64) = 1L in
     let (fitness : MBytes.t list) = [] in
     let (ctxt : Context.t) = Context.empty in
     match
       Storage.prepare ~level ~timestamp ~fitness ctxt
     with
     | Error _ -> assert false
     | Ok (ctxt, _bool) ->
        context := Some ctxt;
        ctxt

let execute_tezos_file filename =
  let contract, contract_hash = read_tezos_file filename in

  let origination = Contract.initial_origination_nonce contract_hash in
  let destination = Contract.originated_contract origination in

  (* TODO: change that. Why do we need a Source opcode in Michelson ? *)
  let source = destination in

  let ctxt = get_context () in

  let (amount : Tez.t) =
    match Tez_repr.of_string !contract_amount with
    | None -> assert false
    | Some amount -> amount in
  let (storage : Script_repr.storage) = {
      Script_repr.storage_type = contract.storage_type;
      Script_repr.storage = !contract_storage;
    } in
  let (arg : Script_repr.expr) = !contract_arg in
  let (qta : int) = 1000 in

  match
    Script_interpreter.execute origination source destination ctxt
                               storage contract amount
                               arg qta
  with
  | Ok (new_storage, result, qta, ctxt, origination) ->
     let ppf = Format.str_formatter in
     let noloc = fun _ -> None in
     Format.fprintf ppf "Result:\n";
     Client_proto_programs.print_expr noloc ppf result;
     Format.fprintf ppf "@.";
     Format.fprintf ppf "Storage:\n";
     Client_proto_programs.print_expr noloc ppf new_storage;
     Format.fprintf ppf "@.";
     let s = Format.flush_str_formatter () in
     Printf.printf "%s\n%!" s;
     contract_storage := new_storage

  | Error errors ->
     Printf.eprintf "%d Errors executing %S\n%!"
                    (List.length errors) filename;
     List.iter (fun error ->
         Format.eprintf "%a" Tezos_context.pp error
       ) errors;
     Tezos_context.pp_print_error Format.err_formatter errors;
     Format.fprintf Format.err_formatter "@.";

     exit 2


let compile_file filename =
  if Filename.check_suffix filename ".liq" then
    compile_liquid_file filename
  else
    if Filename.check_suffix filename ".tz" then
      compile_tezos_file filename
    else begin
        Printf.eprintf "Error: unknown extension for %S\n%!" filename;
        exit 2
      end

let compile_file filename =
  try
    compile_file filename
  with Error ->
       if not !arg_keepon then raise Error

let main () =
  let work_done = ref false in
  let arg_list = Arg.align [
      "-k", Arg.Set arg_keepon, " Continue on error";
      "--no-peephole", Arg.Clear arg_peephole,
      " Disable peephole optimizations";
      "--exec", Arg.String (fun s ->
                    work_done := true;
                    execute_tezos_file s),
      "FILE.tz Execute Tezos file FILE.tz";
      "--load-arg", Arg.String (fun s ->
                   let content = FileString.read_file s in
                   match LiquidFromTezos.data_of_string content with
                   | None -> assert false
                   | Some data -> contract_arg := data),
      "FILE Use data from file as argument";
      "--load-storage", Arg.String (fun s ->
                       let content = FileString.read_file s in
                       match LiquidFromTezos.data_of_string content with
                       | None -> assert false
                       | Some data -> contract_storage := data),
      "FILE Use data from file as initial storage";
      "--amount", Arg.String (fun s -> contract_amount := s),
      "NNN.00 Number of Tez sent";
                   ] in
  let arg_usage = String.concat "\n" [
"liquidity [OPTIONS] FILES";
"";
"The liquidity compiler can translate files from Liquidity to Michelson";
"and from Michelson to Liquidity. Liquidity files must end with the .liq";
"extension. Michelson files must end with the .tz extension.";
"";
"Available options:";
                                ]
  in
  Arg.parse arg_list (fun s -> work_done := true; compile_file s)
            arg_usage;

  if not !work_done then
    Arg.usage arg_list arg_usage


let execute = Script_interpreter.execute

let () =
  try
    main ()
  with Error ->
    Printf.eprintf "Fatal Error: aborting\n";
    exit 2
