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

let verbosity = ref (try
                   int_of_string (Sys.getenv "LIQUID_VERBOSITY")
                     with
                     | Not_found -> 0
                     | _ -> 1 (* LIQUID_DEBUG not a number *)
                    )

let arg_peephole = ref true
let arg_keepon = ref false
let arg_typeonly = ref false
let arg_parseonly = ref false
let arg_singleline = ref false
let arg_annotmic = ref true

let compile_liquid_file filename =
  let ocaml_ast = LiquidFromOCaml.read_file filename in
  if !verbosity>0 then
  FileString.write_file (filename ^ ".ocaml")
    (LiquidOCamlPrinter.contract_ast ocaml_ast);
  if !arg_parseonly then exit 0;
  let syntax_ast, env = LiquidFromOCaml.translate filename ocaml_ast in
  if !verbosity>0 then
  FileString.write_file (filename ^ ".syntax")
                        (LiquidPrinter.Liquid.string_of_contract
                           syntax_ast);
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true env syntax_ast in
  if !verbosity>0 then
    FileString.write_file (filename ^ ".typed")
      (LiquidPrinter.Liquid.string_of_contract_types
         typed_ast);
  let encoded_ast, to_inline =
    LiquidEncode.encode_contract ~annot:!arg_annotmic env typed_ast in
  if !verbosity>0 then
    FileString.write_file (filename ^ ".encoded")
      (LiquidPrinter.Liquid.string_of_contract
         encoded_ast);
  if !arg_typeonly then exit 0;

  let live_ast = LiquidSimplify.simplify_contract encoded_ast to_inline in
  if !verbosity>0 then
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
  (*  let michelson_ast = LiquidEmit.emit_contract pre_michelson in *)

  let output = filename ^ ".tz" in
  let c = LiquidToTezos.convert_contract pre_michelson in
  let s =
    if !arg_singleline
    then LiquidToTezos.line_of_contract c
    else LiquidToTezos.string_of_contract c
  in
  FileString.write_file output s;
  Printf.eprintf "File %S generated\n%!" output;
  Printf.eprintf "If tezos is compiled, you may want to typecheck with:\n";
  Printf.eprintf "  tezos-client typecheck program %s\n" output


let compile_tezos_file filename =
  let code, contract_hash, loc_table = LiquidToTezos.read_tezos_file filename in

  let c = LiquidFromTezos.convert_contract loc_table code in
  let c = LiquidClean.clean_contract c in
  let c = LiquidInterp.interp c in
  if !arg_parseonly then exit 0;
  if !verbosity>0 then begin
    FileString.write_file  (filename ^ ".dot")
                           (LiquidDot.to_string c);
    let cmd = Ocamldot.dot2pdf_cmd (filename ^ ".dot") (filename ^ ".pdf") in
    if Sys.command cmd <> 0 then
      Printf.eprintf "Warning: could not generate pdf from .dot file\n%!";
  end;
  if !arg_typeonly then exit 0;
  let c = LiquidDecomp.decompile c in
  if !verbosity>0 then
  FileString.write_file  (filename ^ ".liq.pre")
                         (LiquidPrinter.Liquid.string_of_contract c);
  let env = LiquidFromOCaml.initial_env filename in
  let typed_ast = LiquidCheck.typecheck_contract ~warnings:false env c in
  let encode_ast, to_inline = LiquidEncode.encode_contract env typed_ast in
  let live_ast = LiquidSimplify.simplify_contract
      ~decompile:true encode_ast to_inline in
  let untyped_ast = LiquidUntype.untype_contract live_ast in
  (* let untyped_ast = c in *)
  let output = filename ^ ".liq" in
  FileString.write_file  output
                         (try
                            LiquidToOCaml.string_of_structure
                              (LiquidToOCaml.structure_of_contract
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
       if not !arg_keepon then raise e


module Data = struct

  let contract = ref ""
  let parameter = ref ""
  let storage = ref ""

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
           LiquidLoc.report_error error
        | Ok x ->
           Printf.printf "%s: %s\n%!" s x)
              [ "parameter", p; "storage", s ]

end


let main () =
  let work_done = ref false in
  let arg_list = Arg.align [
      "-k", Arg.Set arg_keepon, " Continue on error";
      "--verbose", Arg.Unit (fun () -> incr verbosity), " Increment verbosity";
      "--no-peephole", Arg.Clear arg_peephole,
      " Disable peephole optimizations";
      "--type-only", Arg.Set arg_typeonly, "Stop after type checking";
      "--parse-only", Arg.Set arg_parseonly, "Stop after parsing";
      "--single-line", Arg.Set arg_singleline,
      "Output Michelson on a single line";
      "--annot-mic", Arg.Set arg_annotmic,
      "Annotate Michelson with variable names (default: true)";
      "--compact", Arg.Unit (fun () ->
          arg_annotmic := false;
          arg_singleline := true), "Produce compact Michelson";
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
    LiquidLoc.report_error error;
    exit 2
(*
  | exn ->
     Printf.eprintf "Fatal Error: aborting\n";
     Printf.eprintf "  Exception: %s\n%!" (Printexc.to_string exn);
    exit 2
 *)
