(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

(* TODO: We could use a simplify pass to propagate the no-effect
   defining expression of once-used variables to their uniq use
   site. It could dramatically decrease the size of the stack.  *)


open LiquidTypes

module DebugPrint = LiquidPrinter.LiquidDebug

exception Bad_arg

(* We use the parser of the OCaml compiler parser to parse the file,
   we then translate it to a simplified AST, before compiling it
   to Michelson. No type-checking yet.
*)

let compile_liquid_files files =
  let ocaml_asts = List.map (fun filename ->
      let ocaml_ast = LiquidFromParsetree.read_file filename in
      if !LiquidOptions.verbosity>0 then
        FileString.write_file (filename ^ ".ocaml")
          (LiquidOCamlPrinter.contract_ast ocaml_ast);
      (filename, ocaml_ast)
    ) files in
  if !LiquidOptions.parseonly then exit 0;
  let syntax_ast = LiquidFromParsetree.translate_multi ocaml_asts in
  let outprefix =
    match !LiquidOptions.output with
    | Some output when output <> "-" -> Filename.chop_extension output
    | _ -> match List.rev ocaml_asts with
      | [] -> assert false
      | (filename, _) :: _ ->
        Filename.(concat (dirname filename) @@
                  String.uncapitalize_ascii (syntax_ast.contract_name)) in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (outprefix ^ ".syntax")
      (DebugPrint.string_of_contract
         syntax_ast);
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true ~decompiling:false syntax_ast in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (outprefix ^ ".typed")
      (DebugPrint.string_of_contract_types
         typed_ast);
  let encoded_ast, to_inline =
    LiquidEncode.encode_contract ~annot:true typed_ast in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (outprefix ^ ".encoded")
      (DebugPrint.string_of_contract
         encoded_ast);
  if !LiquidOptions.typeonly then exit 0;

  let live_ast =
    if !LiquidOptions.simplify then begin
      let to_inline = if !LiquidOptions.inline then to_inline
        else StringMap.empty in
      let live_ast = LiquidSimplify.simplify_contract encoded_ast to_inline in
      if !LiquidOptions.verbosity>0 then
        FileString.write_file (outprefix ^ ".simple")
          (DebugPrint.string_of_contract
             live_ast);
      live_ast end
    else encoded_ast in

  let pre_michelson = LiquidMichelson.translate live_ast in

  let pre_michelson =
    if !LiquidOptions.peephole then
      LiquidPeephole.simplify pre_michelson
    else
      pre_michelson
  in
  (*  let michelson_ast = LiquidEmit.emit_contract pre_michelson in *)

  (* Output initial(izer/value) *)
  begin match live_ast.c_init with
    | None -> ()
    | Some init ->
      match LiquidInit.compile_liquid_init live_ast.ty_env
              (full_sig_of_contract syntax_ast) init with
      | LiquidInit.Init_constant c_init when !LiquidOptions.json ->
        let c_init = LiquidMichelson.compile_const c_init in
        let s = LiquidToTezos.(json_of_const @@ convert_const ~expand:true c_init) in
        let output = outprefix ^ ".init.json" in
        FileString.write_file output s;
        Printf.eprintf "Constant initial storage generated in %S\n%!" output
      | LiquidInit.Init_constant c_init ->
        let c_init = LiquidMichelson.compile_const c_init in
        let s = LiquidToTezos.(string_of_const @@ convert_const ~expand:false c_init) in
        let output = outprefix ^ ".init.tz" in
        FileString.write_file output s;
        Printf.eprintf "Constant initial storage generated in %S\n%!" output
      | LiquidInit.Init_code (_, pre_init) ->
        let mic_init, _ = LiquidToTezos.convert_contract ~expand:true pre_init in
        let s, output =
          if !LiquidOptions.json then
            LiquidToTezos.json_of_contract mic_init,
            outprefix ^ ".initializer.tz.json"
          else
            LiquidToTezos.string_of_contract mic_init,
            outprefix ^ ".initializer.tz"
        in
        FileString.write_file output s;
        Printf.eprintf "Storage initializer generated in %S\n%!" output
  end;

  let c, loc_table =
    LiquidToTezos.convert_contract ~expand:(!LiquidOptions.json) pre_michelson
  in

  if !LiquidOptions.json then
    let s = LiquidToTezos.json_of_contract c in
    let output = match !LiquidOptions.output with
      | Some output -> output
      | None -> outprefix ^ ".tz.json" in
    match output with
    | "-" -> Printf.printf "%s%!" s
    | _ ->
      FileString.write_file output s;
      Printf.eprintf "File %S generated\n%!" output;
      Printf.eprintf "If you have a node running, \
                      you may want to typecheck with:\n";
      Printf.eprintf "  curl http://127.0.0.1:8732/chains/main/blocks/head/\
                      helpers/scripts/typecheck_code -H \
                      \"Content-Type:application/json\" \
                      -d '{\"program\":'$(cat %s)'}'\n" output
  else
    let s =
      if !LiquidOptions.singleline
      then LiquidToTezos.line_of_contract c
      else LiquidToTezos.string_of_contract c in
    match
      match !LiquidOptions.output with
      | Some output -> output
      | None -> outprefix ^ ".tz"
    with
    | "-" -> Printf.printf "%s%!" s
    | output ->
      FileString.write_file output s;
      Printf.eprintf "File %S generated\n%!" output;
      Printf.eprintf "If tezos is compiled, you may want to typecheck with:\n";
      Printf.eprintf "  tezos-client typecheck script %s\n" output


let compile_tezos_file filename =
  let code, env =
    if Filename.check_suffix filename ".json" || !LiquidOptions.json then
      LiquidToTezos.read_tezos_json filename
    else LiquidToTezos.read_tezos_file filename
  in
  let c = LiquidFromTezos.convert_contract env code in
  let c = LiquidClean.clean_contract c in
  (* let c = if !LiquidOptions.peephole then LiquidPeephole.simplify c else c in *)
  let c = LiquidInterp.interp c in
  if !LiquidOptions.parseonly then exit 0;
  if !LiquidOptions.verbosity>0 then begin
    FileString.write_file  (filename ^ ".dot")
      (LiquidDot.to_string c);
    let cmd = Ocamldot.dot2pdf_cmd (filename ^ ".dot") (filename ^ ".pdf") in
    if Sys.command cmd <> 0 then
      Printf.eprintf "Warning: could not generate pdf from .dot file\n%!";
  end;
  if !LiquidOptions.typeonly then exit 0;

  let c1 = LiquidDecomp.decompile env c in
  let outprefix = match !LiquidOptions.output with
    | Some output when output <> "-" -> Filename.chop_extension output
    | _ -> Filename.(concat (dirname filename) @@
                     String.uncapitalize_ascii c1.contract_name)  ^ ".tz" in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file  (filename ^ ".pre")
      (DebugPrint.string_of_contract c1);
  let typed_ast =
    try
      LiquidCheck.typecheck_contract ~warnings:false ~decompiling:true c1
    with LiquidError _ ->
      (* Retry with generalization of types *)
      LiquidTezosTypes.set_generalize_types env true;
      ignore (LiquidFromTezos.convert_contract env code);
      (* for side effects in generalized type definitions *)
      let c2 = LiquidDecomp.decompile env c in
      if !LiquidOptions.verbosity>0 then
        FileString.write_file  (filename ^ ".pre")
          (DebugPrint.string_of_contract c2);
      LiquidCheck.typecheck_contract ~warnings:false ~decompiling:true c2
  in
  let annoted_tz, type_annots, types = LiquidFromTezos.infos_env env in
  let encode_ast, to_inline =
    LiquidEncode.encode_contract ~decompiling:true typed_ast in
  let live_ast = LiquidSimplify.simplify_contract
      ~decompile_annoted:annoted_tz encode_ast to_inline in
  let multi_ast = LiquidDecode.decode_contract live_ast in
  let untyped_ast = LiquidUntype.untype_contract multi_ast in
  (* let untyped_ast = c in *)
  let output = match !LiquidOptions.output with
    | Some output -> output
    | None -> outprefix ^
              if !LiquidOptions.ocaml_syntax then ".liq" else ".reliq" in
  let s = try
      LiquidPrinter.Syntax.string_of_structure
        (LiquidToParsetree.structure_of_contract ~type_annots ~types untyped_ast) []
    with LiquidError _ ->
      DebugPrint.string_of_contract untyped_ast in
  match output with
  | "-" ->
    Format.printf "%s%!" s
  | _ ->
    FileString.write_file output s;
    Printf.eprintf "File %S generated\n%!" output;
    ()

let report_error = function
  | LiquidError error ->
    LiquidLoc.report_error Format.err_formatter error;
  | LiquidNamespace.Unknown_namespace (p, err_loc) ->
    LiquidLoc.report_error Format.err_formatter
      { err_loc;
        err_msg =
          Printf.sprintf "Unknown module or contract %s" (String.concat "." p) };
  | LiquidFromTezos.Missing_program_field f ->
    Format.eprintf "Missing script field %s@." f;
  | LiquidDeploy.RequestError (code, msg) ->
    Format.eprintf "Request Error (code %d):\n%s@." code msg;
  | LiquidDeploy.ResponseError msg ->
    Format.eprintf "Response Error:\n%s@." msg;
  | LiquidDeploy.RuntimeError (error, _trace) ->
    LiquidLoc.report_error ~kind:"Runtime error" Format.err_formatter error;
  | LiquidDeploy.LocalizedError error ->
    LiquidLoc.report_error ~kind:"Error" Format.err_formatter error;
  | LiquidDeploy.RuntimeFailure (error, None, _trace) ->
    LiquidLoc.report_error ~kind:"Failed at runtime" Format.err_formatter error;
  | LiquidDeploy.RuntimeFailure (error, Some s, _trace) ->
    LiquidLoc.report_error ~kind:"Failed at runtime" Format.err_formatter error;
    Format.eprintf "Failed with %s@." s;
  | Failure f ->
    Format.eprintf "Failure: %s@." f
  | Syntaxerr.Error (Syntaxerr.Other loc) ->
    LiquidLoc.report_error ~kind:"Syntax error" Format.err_formatter
      { err_loc = LiquidLoc.loc_of_location loc; err_msg = "unknown" };
  | exn ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf "Error: %s\nBacktrace:\n%s@."
      (Printexc.to_string exn) backtrace

let compile_tezos_file filename =
  try compile_tezos_file filename
  with exn ->
    (* Rety and ignore annotations if failing *)
    if !LiquidOptions.retry_without_annots ||
       !LiquidOptions.ignore_annots then
      raise exn;
    report_error exn;
    Format.printf
      "Decompilation failed, retrying and ignoring \
       Michelson type annotations@.";
    LiquidOptions.ignore_annots := true;
    compile_tezos_file filename;
    LiquidOptions.ignore_annots := false

let compile_tezos_files = List.iter compile_tezos_file


module Data = struct

  let files = ref []
  let parameter = ref ""
  let storage = ref ""
  let entry_name = ref "main"

  let contract_address = ref ""
  let init_inputs = ref []

  let get_files () =
    let l = List.rev !files in
    if l = [] then raise Bad_arg;
    l

  let register_deploy_input s =
    init_inputs := s :: !init_inputs

  let get_inputs () = List.rev !init_inputs

  let validate_contract_addr s =
    if String.length s <> 36 || String.sub s 0 3 <> "KT1" then
      failwith (s ^ " is not a valid contract address")

  let validate_key_hash s =
    if String.length s <> 36 || String.sub s 0 2 <> "tz" then
      failwith (s ^ " is not a valid key hash")

  let validate_private_key s =
    if (String.length s <> 54 || let p = String.sub s 0 4 in
        p <> "edsk" && p <> "spsk" && p <> "p2sk") then
      failwith (s ^ " is not a valid private key")
end

let compile_files () =
  let files = Data.get_files () in
  let liq_files, others =
    List.partition (fun filename ->
        Filename.check_suffix filename ".liq" ||
        Filename.check_suffix filename ".reliq" )
      files in
  let tz_files, unknown =
    List.partition (fun filename ->
        Filename.check_suffix filename ".tz" ||
        Filename.check_suffix filename ".json") others in
  match unknown with
    [] ->
    begin match liq_files, tz_files with
      | [], [] ->
        Format.eprintf "No files given as arguments@.";
        raise Bad_arg
      | [], _ ->
        compile_tezos_files tz_files
      | _, _ ->
        compile_liquid_files liq_files;
        compile_tezos_files tz_files
    end
  | _ ->
    Format.eprintf "Error: unknown extension for files: %a@."
      (Format.pp_print_list Format.pp_print_string) unknown;
    exit 2

let translate () =
  let files = Data.get_files () in
  let parameter = !Data.parameter in
  let storage = !Data.storage in
  let entry_name = !Data.entry_name in
  let ocaml_asts = List.map (fun f -> f, LiquidFromParsetree.read_file f) files in
  (* first, extract the types *)
  let contract = LiquidFromParsetree.translate_multi ocaml_asts in
  let _ = LiquidCheck.typecheck_contract ~warnings:true contract in
  let contract_sig = full_sig_of_contract contract in
  let entry =
    try
      List.find (fun e -> e.entry_sig.entry_name = entry_name)
        contract.entries
    with Not_found ->
      Format.eprintf "Contract has no entry point %s@." entry_name; exit 2
  in
  let input =
    LiquidData.translate { contract.ty_env with filename = "parameter" }
      contract_sig parameter
      entry.entry_sig.parameter in
  let parameter_const = match contract_sig.f_entries_sig with
    | [_] -> input
    | _ -> LiquidEncode.encode_const contract.ty_env contract_sig
             (CConstr (prefix_entry ^ entry_name,
                       (LiquidDecode.decode_const input))) in
  let to_str mic_data =
    let mic_data = LiquidMichelson.compile_const mic_data in
    if !LiquidOptions.json then
      LiquidToTezos.(json_of_const @@ convert_const ~expand:true mic_data)
    else
      LiquidToTezos.(line_of_const @@ convert_const ~expand:false mic_data) in
  if storage = "" then
    (* Only translate parameter *)
    Printf.printf "%s\n%!" (to_str parameter_const)
  else
    let storage_const =
      LiquidData.translate { contract.ty_env with filename = "storage" }
        contract_sig storage contract.storage in
    if !LiquidOptions.json then
      Printf.printf "{\n  \"parameter\": %s; \n  \"storage\": %s\n}\n%!"
        (to_str parameter_const) (to_str storage_const)
    else
      Printf.printf "parameter: %s \nstorage: %s\n%!"
        (to_str parameter_const) (to_str storage_const)

let inject file =
  let signature = match !LiquidOptions.signature with
    | None ->
      Printf.eprintf "Error: missing --signature option for --inject\n%!";
      exit 2
    | Some signature -> signature
  in
  (* an hexa encoded operation *)
  let operation = FileString.read_file file in
  let op_h = LiquidDeploy.Sync.inject ~operation ~signature in
  Printf.printf "Operation injected: %s\n%!" op_h

let run () =
  let open LiquidDeploy in
  let ops, r_storage, big_map_diff =
    Sync.run (From_files (Data.get_files ()))
      !Data.entry_name !Data.parameter !Data.storage
  in
  Printf.printf "%s\n# Internal operations: %d\n%!"
    (LiquidPrinter.Liquid.string_of_const r_storage)
    (List.length ops);
  match big_map_diff with
  | None -> ()
  | Some diff ->
    Printf.printf "\nBig map diff:\n";
    List.iter (function
        | Big_map_add (k, v) ->
          Printf.printf "+  %s --> %s\n"
            (match k with
             | DiffKeyHash h -> h
             | DiffKey k -> LiquidPrinter.Liquid.string_of_const k)
            (LiquidPrinter.Liquid.string_of_const v)
        | Big_map_remove k ->
          Printf.printf "-  %s\n"
            (match k with
             | DiffKeyHash h -> h
             | DiffKey k -> LiquidPrinter.Liquid.string_of_const k)
      ) diff;
    Printf.printf "%!"


let forge_deploy () =
  let op =
    LiquidDeploy.Sync.forge_deploy
      ~delegatable:!LiquidOptions.delegatable
      ~spendable:!LiquidOptions.spendable
      (LiquidDeploy.From_files (Data.get_files ())) (Data.get_inputs ())
  in
  Printf.eprintf "Raw operation:\n--------------\n%!";
  Printf.printf "%s\n%!" op

let init_storage () =
  let storage =
    LiquidDeploy.Sync.init_storage
      (LiquidDeploy.From_files (Data.get_files ())) (Data.get_inputs ())
  in
  let outname =
    let c = match !LiquidOptions.main with
      | Some c -> c
      | None -> match List.rev (Data.get_files ()) with
        | c :: _ -> c
        | [] -> assert false in
    String.uncapitalize_ascii c in
  let storage = LiquidMichelson.compile_const storage in
  if !LiquidOptions.json then
    let s = LiquidToTezos.(json_of_const @@ convert_const ~expand:true storage) in
    let output = match !LiquidOptions.output with
      | Some output -> output
      | None -> outname ^ ".init.json" in
    FileString.write_file output s;
    Printf.printf "Constant initial storage generated in %S\n%!" output
  else
    let s = LiquidToTezos.(line_of_const @@ convert_const ~expand:false storage) in
    let output = match !LiquidOptions.output with
      | Some output -> output
      | None -> outname ^ ".init.tz" in
    FileString.write_file output s;
    Printf.printf "Constant initial storage generated in %S\n%!" output

let deploy () =
  match
    LiquidDeploy.Sync.deploy
      ~delegatable:!LiquidOptions.delegatable
      ~spendable:!LiquidOptions.spendable
      (LiquidDeploy.From_files (Data.get_files ())) (Data.get_inputs ())
  with
  | op_h, Ok contract_id ->
    Printf.printf "New contract %s deployed in operation %s\n%!"
      contract_id op_h
  | op_h, Error e ->
    Printf.printf "Failed deployment in operation %s\n%!" op_h;
    raise e

let get_storage () =
  let r_storage =
    LiquidDeploy.Sync.get_storage
      (LiquidDeploy.From_files (Data.get_files ()))
      !Data.contract_address
  in
  Printf.printf "%s\n%!"
    (LiquidPrinter.Liquid.string_of_const r_storage)

let call_arg () =
  let s =
    LiquidDeploy.forge_call_arg
      (LiquidDeploy.From_files (Data.get_files ()))
      ~entry_name:!Data.entry_name
      !Data.parameter
  in
  match !LiquidOptions.output with
  | None ->
    Printf.printf "Use --arg '%s'\n%!" s
  | Some "-" ->
    Printf.printf "'%s'%!" s
  | Some file ->
    FileString.write_file file s

let call () =
  match
    LiquidDeploy.Sync.call
      (LiquidDeploy.From_files (Data.get_files ()))
      !Data.contract_address
      !Data.entry_name
      !Data.parameter
  with
  | op_h, Ok () ->
    Printf.printf "Successful call to contract %s in operation %s\n%!"
      !Data.contract_address op_h
  | op_h, Error e ->
    Printf.printf "Failed call to contract %s in operation %s\n%!"
      !Data.contract_address op_h;
    raise e

let forge_call () =
  let op =
    LiquidDeploy.Sync.forge_call
      (LiquidDeploy.From_files (Data.get_files ()))
      !Data.contract_address
      !Data.entry_name
      !Data.parameter in
  Printf.eprintf "Raw operation:\n--------------\n%!";
  Printf.printf "%s\n%!" op

let pack const ty =
  let liquid =
    try Some (LiquidDeploy.From_files (Data.get_files ()))
    with Bad_arg -> None in
  let bytes =
    LiquidDeploy.Sync.pack ?liquid ~const ~ty in
  Printf.printf "%s\n%!" bytes

let parse_tez_to_string expl amount =
  match LiquidData.translate (LiquidFromParsetree.initial_env expl)
          dummy_contract_sig amount Ttez
  with
  | CTez t ->
    let mutez = match t.mutez with
      | Some mutez -> mutez
      | None  -> "000000"
    in
    t.tezzies ^ mutez
  | _ -> assert false


let convert_file filename =
  let is_liq = Filename.check_suffix filename ".liq" in
  let is_re =  Filename.check_suffix filename ".reliq" in
  if not (is_liq || is_re) then
    Printf.kprintf
      failwith "Error: don't know what to do with filename %S" filename;

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  LiquidOptions.ocaml_syntax := is_liq;
  let str, comments = LiquidParse.implementation lexbuf in
  (* ocamldoc comments are already in ast *)
  let comments = List.filter (fun (s, _) ->
      String.length s > 0 && s.[0] <> '*') comments in
  LiquidOptions.ocaml_syntax := not is_liq;
  let s = LiquidPrinter.Syntax.string_of_structure str comments in
  LiquidOptions.ocaml_syntax := is_liq;
  Printf.printf "%s%!" s;
  ()

let main () =
  let work_done = ref false in
  let arg_list = Arg.align [
      "--verbose", Arg.Unit (fun () -> incr LiquidOptions.verbosity),
      " Increment verbosity";
      "-v", Arg.Unit (fun () -> incr LiquidOptions.verbosity),
      " Increment verbosity";

      "--re", Arg.Clear LiquidOptions.ocaml_syntax, " Use ReasonML syntax";
      "--convert", Arg.String (fun s ->
          convert_file s;
          work_done := true), " Switch between OCaml and ReasonML syntax (stdout)";

      "--version", Arg.Unit (fun () ->
          Format.printf "%s" LiquidToParsetree.output_version;
          if !LiquidOptions.verbosity > 0 then
            Format.printf " (%s)" LiquidVersion.commit;
          if !LiquidOptions.verbosity > 1 then
            Format.printf "\nCompiled on %s" LiquidVersion.en_date;
          Format.printf "@.";
          exit 0
        ),
      " Show version and exit";

      "-o", Arg.String (fun o -> LiquidOptions.output := Some o),
      "<filename> Output code in <filename>";

      "--main", Arg.String (fun main -> LiquidOptions.main := Some main),
      "<ContractName> Produce code for contract named <ContractName>";

      "--no-inline", Arg.Clear LiquidOptions.inline,
      " Disable inlining";

      "--no-simplify", Arg.Clear LiquidOptions.simplify,
      " Disable simplifications";

      "--no-peephole", Arg.Clear LiquidOptions.peephole,
      " Disable peephole optimizations";

      "--type-only", Arg.Set LiquidOptions.typeonly,
      " Stop after type checking";

      "--parse-only", Arg.Set LiquidOptions.parseonly,
      " Stop after parsing";

      "--compact", Arg.Set LiquidOptions.singleline,
      " Produce compact Michelson";

      "--no-annot", Arg.Set LiquidOptions.no_annot,
      " Don't produce any annotations when compiling";

      "--no-ignore-annots", Arg.Clear LiquidOptions.retry_without_annots,
      " Don't ignore annotations of failure when decompiling";

      "--no-uncurry", Arg.Set LiquidOptions.no_uncurrying,
      " Don't uncurry non partially applied lambdas";

      "--json", Arg.Set LiquidOptions.json,
      " Output Michelson in JSON representation";

      "--amount", Arg.String (fun amount ->
          LiquidOptions.amount := parse_tez_to_string "--amount" amount
        ),
      "<1.99tz> Set amount for deploying or running a contract (default: 0tz)";

      "--fee", Arg.String (fun fee ->
          LiquidOptions.fee := parse_tez_to_string "--fee" fee
        ),
      "<0.05tz> Set fee for deploying a contract (default: 0.05tz)";

      "--source", Arg.String (fun s ->
          Data.validate_key_hash s;
          LiquidOptions.source := Some s),
      "<tz1...> Set the source for deploying or running a contract (default: none)";

      "--private-key", Arg.String (fun s ->
          Data.validate_private_key s;
          LiquidOptions.private_key := Some s),
      "<edsk...> Set the private key for deploying a contract (default: none)";

      "--counter", Arg.Int (fun n -> LiquidOptions.counter := Some n),
      "N Set the counter for the operation instead of retrieving it";

      "--tezos-node", Arg.String (fun s -> LiquidOptions.tezos_node := s),
      "<addr:port> Set the address and port of a Tezos node to run or deploy \
       contracts (default: 127.0.0.1:8732)\
       \n\
       \n\
       Available commands:\
      ";

      "--protocol", Arg.String (function
          | "zeronet" -> LiquidOptions.protocol := Some Zeronet
          | "alphanet" -> LiquidOptions.protocol := Some Alphanet
          | "mainnet" -> LiquidOptions.protocol := Some Mainnet
          | s ->
            Format.eprintf
              "Unknown protocol %s (use mainnet, zeronet, alphanet)@." s;
            exit 2
        ),
      " Specify protocol (mainnet, zeronet, alphanet) \
       (detect if not specified)";

      "--run", Arg.Tuple [
        Arg.String (fun s -> Data.entry_name := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.String (fun s -> Data.storage := s);
        Arg.Unit (fun () ->
            work_done := true;
            run ());
      ],
      "ENTRY PARAMETER STORAGE Run Liquidity contract on Tezos node";

      "--delegatable", Arg.Set LiquidOptions.delegatable,
      " With --[forge-]deploy, deploy a delegatable contract";

      "--spendable", Arg.Set LiquidOptions.spendable,
      " With --[forge-]deploy, deploy a spendable contract";

      "--init-storage", Arg.Tuple [
        Arg.Rest Data.register_deploy_input;
        Arg.Unit (fun () ->
            work_done := true;
            init_storage ());
      ],
      " [INPUT1 INPUT2 ...] Generate initial storage";

      "--forge-deploy", Arg.Tuple [
        Arg.Rest Data.register_deploy_input;
        Arg.Unit (fun () ->
            work_done := true;
            forge_deploy ());
      ],
      " [INPUT1 INPUT2 ...] Forge deployment operation for contract";

      "--deploy", Arg.Tuple [
        Arg.Rest Data.register_deploy_input;
        Arg.Unit (fun () ->
            work_done := true;
            deploy ());
      ],
      " [INPUT1 INPUT2 ...] Deploy contract";

      "--get-storage", Arg.Tuple [
        Arg.String (fun s ->
            Data.validate_contract_addr s;
            Data.contract_address := s);
        Arg.Unit (fun () ->
            work_done := true;
            get_storage ());
      ],
      "<KT1...> Get deployed contract storage";

      "--call", Arg.Tuple [
        Arg.String (fun s ->
            Data.validate_contract_addr s;
            Data.contract_address := s);
        Arg.String (fun s -> Data.entry_name := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.Unit (fun () ->
            work_done := true;
            call ());
      ],
      "<KT1...> ENTRY PARAMETER Call deployed contract";

      "--call-arg", Arg.Tuple [
        Arg.String (fun s -> Data.entry_name := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.Unit (fun () ->
            work_done := true;
            call_arg ());
      ],
      "ENTRY PARAMETER Call deployed contract";

      "--forge-call", Arg.Tuple [
        Arg.String (fun s ->
            Data.validate_contract_addr s;
            Data.contract_address := s);
        Arg.String (fun s -> Data.entry_name := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.Unit (fun () ->
            work_done := true;
            forge_call ());
      ],
      "<KT1...> ENTRY PARAMETER Forge call transaction operation";

      "--data",
      (let data_args = ref [] in
       Arg.Tuple [
         Arg.String (fun s -> Data.entry_name := s);
         Arg.Rest (fun s -> data_args := s :: !data_args);
         Arg.Unit (fun () ->
             begin match !data_args with
               | [p] -> Data.parameter := p
               | [s; p] -> Data.parameter := p; Data.storage := s
               | _ -> raise Bad_arg
             end;
             work_done := true;
             translate ());
       ]),
      "ENTRY PARAMETER [STORAGE] Translate to Michelson";

      "--pack",
      (let const = ref "" in
       let ty = ref "" in
       Arg.Tuple [
         Arg.String (fun s -> const := s);
         Arg.String (fun s -> ty := s);
         Arg.Unit (fun () ->
             work_done := true;
             pack !const !ty);
       ]),
      "DATA TYPE Pack (serialize) data of type TYPE";

      "--signature", Arg.String (fun s -> LiquidOptions.signature := Some s),
      "SIGNATURE Set the signature for an operation";

      "--inject", Arg.String (fun op ->
          work_done := true;
          inject op
        ), "OPERATION.bytes Inject a sign operation\n\nMisc:";


    ]
  in
  let arg_usage = String.concat "\n" [
      "liquidity [OPTIONS] FILES [COMMAND]";
      "";
      "The liquidity compiler can translate files from Liquidity to Michelson";
      "and from Michelson to Liquidity. Liquidity files must end with the .liq";
      "extension. Michelson files must end with the .tz extension.";
      "";
      "Available options:";
    ]
  in
  try
    Arg.parse arg_list (fun s -> Data.files := s :: !Data.files) arg_usage;
    (* if Data.get_files () = [] then raise Bad_arg; *)
    if not !work_done then compile_files ();
  with Bad_arg ->
    Arg.usage arg_list arg_usage


let () =
  Printexc.record_backtrace true;
  try
    main ()
  with exn ->
    report_error exn;
    exit 1
