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


open LiquidTypes

(* We use the parser of the OCaml compiler parser to parse the file,
  we then translate it to a simplified AST, before compiling it
  to Michelson. No type-checking yet.
 *)

let compile_liquid_file filename =
  let ocaml_ast = LiquidFromOCaml.read_file filename in
  if !LiquidOptions.verbosity>0 then
  FileString.write_file (filename ^ ".ocaml")
    (LiquidOCamlPrinter.contract_ast ocaml_ast);
  if !LiquidOptions.parseonly then exit 0;
  let syntax_ast, syntax_init, env =
    LiquidFromOCaml.translate filename ocaml_ast in
  if !LiquidOptions.verbosity>0 then
  FileString.write_file (filename ^ ".syntax")
                        (LiquidPrinter.Liquid.string_of_contract
                           syntax_ast);
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true env syntax_ast in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (filename ^ ".typed")
      (LiquidPrinter.Liquid.string_of_contract_types
         typed_ast);
  let encoded_ast, to_inline =
    LiquidEncode.encode_contract ~annot:!LiquidOptions.annotmic env typed_ast in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (filename ^ ".encoded")
      (LiquidPrinter.Liquid.string_of_contract
         encoded_ast);
  if !LiquidOptions.typeonly then exit 0;

  let live_ast = LiquidSimplify.simplify_contract encoded_ast to_inline in
  if !LiquidOptions.verbosity>0 then
  FileString.write_file (filename ^ ".simple")
                        (LiquidPrinter.Liquid.string_of_contract
                           live_ast);

  let pre_michelson = LiquidMichelson.translate live_ast in

  let pre_michelson =
    if !LiquidOptions.peephole then
      LiquidPeephole.simplify pre_michelson
    else
      pre_michelson
  in
  (*  let michelson_ast = LiquidEmit.emit_contract pre_michelson in *)

  (* Output initial(izer/value) *)
  begin match syntax_init with
  | None -> ()
  | Some syntax_init ->
    match LiquidInit.compile_liquid_init env syntax_ast.contract_sig syntax_init with
    | LiquidInit.Init_constant c_init ->
      let s = LiquidPrinter.Michelson.line_of_const c_init in
      let output = env.filename ^ ".init.tz" in
      FileString.write_file output s;
      Printf.eprintf "Constant initial storage generated in %S\n%!" output
    | LiquidInit.Init_code (_, pre_init) ->
      let mic_init, _ = LiquidToTezos.convert_contract ~expand:true pre_init in
      let s = LiquidToTezos.line_of_contract mic_init in
      let output = env.filename ^ ".initializer.tz" in
      FileString.write_file output s;
      Printf.eprintf "Storage initializer generated in %S\n%!" output
  end;

  let output = filename ^ ".tz" in
  let c, loc_table =
    LiquidToTezos.convert_contract ~expand:false pre_michelson in
  let s =
    if !LiquidOptions.singleline
    then LiquidToTezos.line_of_contract c
    else LiquidToTezos.string_of_contract c
  in
  FileString.write_file output s;
  Printf.eprintf "File %S generated\n%!" output;
  Printf.eprintf "If tezos is compiled, you may want to typecheck with:\n";
  Printf.eprintf "  tezos-client typecheck program %s\n" output


let compile_tezos_file filename =
  let code, env = LiquidToTezos.read_tezos_file filename in

  let c, annoted_tz, type_annots = LiquidFromTezos.convert_contract env code in
  let c = LiquidClean.clean_contract c in
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
  let c = LiquidDecomp.decompile c in
  if !LiquidOptions.verbosity>0 then
  FileString.write_file  (filename ^ ".liq.pre")
                         (LiquidPrinter.Liquid.string_of_contract c);
  let env = LiquidFromOCaml.initial_env filename in
  let typed_ast = LiquidCheck.typecheck_contract ~warnings:false env c in
  let encode_ast, to_inline = LiquidEncode.encode_contract env typed_ast in
  let live_ast = LiquidSimplify.simplify_contract
      ~decompile_annoted:annoted_tz encode_ast to_inline in
  let untyped_ast = LiquidUntype.untype_contract live_ast in
  (* let untyped_ast = c in *)
  let output = filename ^ ".liq" in
  FileString.write_file  output
                         (try
                            LiquidToOCaml.string_of_structure
                              (LiquidToOCaml.structure_of_contract
                                 ~type_annots
                                 untyped_ast)
                          with LiquidError _ ->
                            LiquidPrinter.Liquid.string_of_contract
                              untyped_ast);
  Printf.eprintf "File %S generated\n%!" output;
  ()


let handle_file filename =
  if Filename.check_suffix filename ".liq" then
    compile_liquid_file filename
  else
    if Filename.check_suffix filename ".tz" then
      compile_tezos_file filename
    else begin
        Printf.eprintf "Error: unknown extension for %S\n%!" filename;
        exit 2
      end

let handle_file filename =
  try
    handle_file filename
  with (LiquidError _) as e ->
       if not !LiquidOptions.keepon then raise e


module Data = struct

  let contract = ref ""
  let parameter = ref ""
  let storage = ref ""

  let contract_address = ref ""

  let translate () =
    let filename = !contract in
    let contract = FileString.read_file filename in
    let parameter = !parameter in
    let storage = !storage in
    let p,s = LiquidData.data_of_liq ~filename
                                     ~contract ~parameter ~storage in
    List.iter (fun (s,x) ->
        match x with
        | Error error ->
           LiquidLoc.report_error Format.err_formatter error
        | Ok x ->
           Printf.printf "%s: %s\n%!" s x)
              [ "parameter", p; "storage", s ]

  let run () =
    let r_storage, big_map_diff =
      LiquidDeploy.Sync.run
        (LiquidDeploy.From_file !contract) !parameter !storage
    in
    Printf.printf "%s\n%!"
      (LiquidData.string_of_const r_storage);
    match big_map_diff with
    | None -> ()
    | Some diff ->
      Printf.printf "\nBig map diff:\n";
      List.iter (function
          | LiquidDeploy.Big_map_add (k, v) ->
            Printf.printf "+  %s --> %s\n"
              (LiquidData.string_of_const k)
              (LiquidData.string_of_const v)
          | LiquidDeploy.Big_map_remove k ->
            Printf.printf "-  %s\n"
              (LiquidData.string_of_const k)
        ) diff;
      Printf.printf "%!"


  let init_inputs = ref []

  let register_deploy_input s =
    init_inputs := s :: !init_inputs

  let forge_deploy () =
    let op =
      LiquidDeploy.Sync.forge_deploy
        ~delegatable:!LiquidOptions.delegatable
        ~spendable:!LiquidOptions.spendable
        (LiquidDeploy.From_file !contract) (List.rev !init_inputs)
    in
    Printf.eprintf "Raw operation:\n--------------\n%!";
    Printf.printf "%s\n%!" op

  let deploy () =
    let op_h, contract_id =
      LiquidDeploy.Sync.deploy
        ~delegatable:!LiquidOptions.delegatable
        ~spendable:!LiquidOptions.spendable
        (LiquidDeploy.From_file !contract) (List.rev !init_inputs)
    in
    Printf.printf "New contract %s deployed in operation %s\n%!"
      contract_id op_h

  let get_storage () =
    let r_storage =
      LiquidDeploy.Sync.get_storage (LiquidDeploy.From_file !contract)
        !contract_address
    in
    Printf.printf "%s\n%!"
      (LiquidData.string_of_const r_storage)

  let call () =
    let op_h =
      LiquidDeploy.Sync.call
        (LiquidDeploy.From_file !contract)
        !contract_address
        !parameter
    in
    Printf.printf "Successful call to contract %s (at %s) in operation %s\n%!"
      !contract !contract_address op_h

end

let parse_tez_to_string expl amount =
  match LiquidData.translate (LiquidFromOCaml.initial_env expl)
          dummy_contract_sig amount Ttez
  with
  | CTez t ->
    let mutez = match t.mutez with
      | Some mutez -> mutez
      | None  -> "000000"
    in
    t.tezzies ^ mutez
  | _ -> assert false


let main () =
  let work_done = ref false in
  let arg_list = Arg.align [
      "-k", Arg.Set LiquidOptions.keepon, " Continue on error";

      "--verbose", Arg.Unit (fun () -> incr LiquidOptions.verbosity),
      " Increment verbosity";

      "--version", Arg.Unit (fun () ->
          Format.printf "%s@." LiquidToOCaml.output_version;
          exit 0
        ),
      " Show version and exit";

      "--no-peephole", Arg.Clear LiquidOptions.peephole,
      " Disable peephole optimizations";

      "--type-only", Arg.Set LiquidOptions.typeonly,
      " Stop after type checking";

      "--parse-only", Arg.Set LiquidOptions.parseonly,
      " Stop after parsing";

      "--single-line", Arg.Set LiquidOptions.singleline,
      " Output Michelson on a single line";

      "--no-annot", Arg.Clear LiquidOptions.annotmic,
      " Don't annotate Michelson with variable names";

      "--annot-prim", Arg.Clear LiquidOptions.annotafter,
      " Annotate Michelson primitives directly";

      "--compact", Arg.Unit (fun () ->
          LiquidOptions.annotmic := false;
          LiquidOptions.singleline := true),
      " Produce compact Michelson";

      "--amount", Arg.String (fun amount ->
          LiquidOptions.amount := parse_tez_to_string "--amount" amount
        ),
      "<1.99tz> Set amount for deploying or running a contract (default: 0tz)";

      "--fee", Arg.String (fun fee ->
          LiquidOptions.fee := parse_tez_to_string "--fee" fee
        ),
      "<0.05tz> Set fee for deploying a contract (default: 0.05tz)";

      "--source", Arg.String (fun s -> LiquidOptions.source := Some s),
      "<tz1...> Set the source for deploying or running a contract (default: none)";

      "--private-key", Arg.String (fun s -> LiquidOptions.private_key := Some s),
      "<edsk...> Set the private key for deploying a contract (default: none)";

      "--tezos-node", Arg.String (fun s -> LiquidOptions.tezos_node := s),
      "<addr:port> Set the address and port of a Tezos node to run or deploy \
       contracts (default: 127.0.0.1:8732)";

      "--run", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.String (fun s -> Data.storage := s);
        Arg.Unit (fun () ->
            work_done := true;
            Data.run ());
      ],
      "FILE.liq PARAMETER STORAGE Run Liquidity contract on Tezos node";

      "--delegatable", Arg.Set LiquidOptions.delegatable,
      " With --[forge-]deploy, deploy a delegatable contract";

      "--spendable", Arg.Set LiquidOptions.spendable,
      " With --[forge-]deploy, deploy a spendable contract";

      "--forge-deploy", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.Rest Data.register_deploy_input;
        Arg.Unit (fun () ->
            work_done := true;
            Data.forge_deploy ());
      ],
      "FILE.liq INPUT1 INPUT2 ... Forge deployment operation for contract";

      "--deploy", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.Rest Data.register_deploy_input;
        Arg.Unit (fun () ->
            work_done := true;
            Data.deploy ());
      ],
      "FILE.liq INPUT1 INPUT2 ... Deploy contract";

      "--get-storage", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.String (fun s -> Data.contract_address := s);
        Arg.Unit (fun () ->
            work_done := true;
            Data.get_storage ());
      ],
      "FILE.liq <TZ1...> Get deployed contract storage";

      "--call", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.String (fun s -> Data.contract_address := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.Unit (fun () ->
            work_done := true;
            Data.call ());
      ],
      "FILE.liq <TZ1...> PARAMETER Call deployed contract";

      "--data", Arg.Tuple [
        Arg.String (fun s -> Data.contract := s);
        Arg.String (fun s -> Data.parameter := s);
        Arg.String (fun s -> Data.storage := s);
        Arg.Unit (fun () ->
            work_done := true;
            Data.translate ());
      ],
      "FILE.liq PARAMETER STORAGE Translate to Michelson";

    ] @ LiquidToTezos.arg_list work_done

  in
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
  Arg.parse arg_list (fun s -> work_done := true; handle_file s)
    arg_usage;

  if not !work_done then
    Arg.usage arg_list arg_usage


let () =
  Printexc.record_backtrace true;
  try
    main ()
  with
  | LiquidError error ->
    LiquidLoc.report_error Format.err_formatter error;
    exit 1
  | LiquidFromTezos.Missing_program_field f ->
    Format.eprintf "Missing script field %s@." f;
    exit 1
  | LiquidDeploy.RequestError (code, msg) ->
    Format.eprintf "Request Error (code %d):\n%s@." code msg;
    exit 1
  | LiquidDeploy.ResponseError msg ->
    Format.eprintf "Response Error:\n%s@." msg;
    exit 1
  | LiquidDeploy.RuntimeError error ->
    LiquidLoc.report_error ~kind:"Runtime error" Format.err_formatter error;
    exit 1
  | LiquidDeploy.LocalizedError error ->
    LiquidLoc.report_error ~kind:"Error" Format.err_formatter error;
    exit 1
  | LiquidDeploy.RuntimeFailure (error, None) ->
    LiquidLoc.report_error ~kind:"Failed at runtime" Format.err_formatter error;
    exit 1
  | LiquidDeploy.RuntimeFailure (error, Some s) ->
    LiquidLoc.report_error ~kind:"Failed at runtime" Format.err_formatter error;
    Format.eprintf "Failed with %S@." s;
    exit 1
  | Failure f ->
    Format.eprintf "Failure: %s@." f;
    exit 2
(*
  | exn ->
     Printf.eprintf "Fatal Error: aborting\n";
     Printf.eprintf "  Exception: %s\n%!" (Printexc.to_string exn);
    exit 2
 *)
