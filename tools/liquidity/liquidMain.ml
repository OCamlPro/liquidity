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

let arg_peephole = ref true
let arg_keepon = ref false

let compile_liquid_file filename =
  let ocaml_ast = LiquidFromOCaml.read_file filename in
  let syntax_ast, env = LiquidFromOCaml.translate filename ocaml_ast in
  FileString.write_file (filename ^ ".syntax")
                        (LiquidPrinter.Liquid.string_of_contract
                           syntax_ast);
  let typed_ast, to_inline = LiquidCheck.types ~warnings:true env syntax_ast in
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
                        (LiquidToTezos.string_of_contract
                           (LiquidToTezos.convert_contract pre_michelson));
  Printf.eprintf "File %S generated\n%!" output;
  Printf.eprintf "Typecheck with:\n  tezos-client typecheck program %s\n" output


let compile_tezos_file filename =
  let code, contract_hash = LiquidToTezos.read_tezos_file filename in

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
  let typed_ast, to_inline = LiquidCheck.types ~warnings:false env c in
  (*  Printf.eprintf "Inlining: %d\n%!" (StringMap.cardinal to_inline); *)
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
  Arg.parse arg_list (fun s -> work_done := true; compile_file s)
            arg_usage;

  if not !work_done then
    Arg.usage arg_list arg_usage


let () =
  try
    main ()
  with Error ->
    Printf.eprintf "Fatal Error: aborting\n";
    exit 2
