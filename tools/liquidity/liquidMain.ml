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
  let typed_ast, to_inline =
    LiquidCheck.typecheck_contract ~warnings:true env syntax_ast in
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
  (*  let michelson_ast = LiquidEmit.emit_contract pre_michelson in *)

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
  let typed_ast, to_inline =
    LiquidCheck.typecheck_contract ~warnings:false env c in
  (*  Printf.eprintf "Inlining: %d\n%!" (StringMap.cardinal to_inline); *)
  let live_ast = LiquidSimplify.simplify_contract typed_ast to_inline in
  let untyped_ast = LiquidUntype.untype_contract live_ast in
  let output = filename ^ ".liq" in
  FileString.write_file  output
                         (try
                            LiquidToOCaml.string_of_structure
                              (LiquidToOCaml.structure_of_contract
                                 untyped_ast)
                          with Error _ ->
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
  with (Error _) as e ->
       if not !arg_keepon then raise e


module Data = struct

  let rec translate_const_exp loc exp =
    match exp.desc with
    | Let (_, loc, _, _) ->
       LiquidLoc.raise_error ~loc "'let' forbidden in constant"
    | Const (ty, c) -> c

    | Record (_, _)
      | Constructor (_, _, _)
      | Apply (Prim_tuple, _, _)
      | Apply (Prim_neq, _, [_])
      | Apply (Prim_Left, _, [_])
      | Apply (Prim_Right, _, [_])
      | Apply (Prim_Some, _, [_])
      | Apply (Prim_Cons, _, [_])
      -> LiquidLoc.raise_error "<not yet implemented>"

    | Apply (_, _, _)
      | Var (_, _, _)
      | SetVar (_, _, _, _)
      | If (_, _, _)
      | Seq (_, _)
      | LetTransfer (_, _, _, _, _, _, _, _)
      | MatchOption (_, _, _, _, _)
      | MatchList (_, _, _, _, _, _)
      | Loop (_, _, _, _)
      | Lambda (_, _, _, _, _)
      | MatchVariant (_, _, _)
      ->
       LiquidLoc.raise_error ~loc "non-constant expression"

  let data_of_liq ~contract ~parameter ~storage =
    (* first, extract the types *)
    let ocaml_ast = LiquidFromOCaml.structure_of_string contract in
    let contract, env = LiquidFromOCaml.translate "buffer" ocaml_ast in
    let _, _ = LiquidCheck.typecheck_contract
                 ~warnings:true env contract in

    let translate name s ty =
      let ml_exp = LiquidFromOCaml.expression_of_string s in
      let sy_exp = LiquidFromOCaml.translate_expression "buffer" ml_exp in
      let ty_exp =
        LiquidCheck.typecheck_code ~warnings:true env contract ty sy_exp in
      let loc = LiquidLoc.loc_in_file name in
      let ty_exp = translate_const_exp loc ty_exp in
      let s = LiquidPrinter.Michelson.string_of_const ty_exp in
      Some s
    in
    (translate "parameter" parameter contract.parameter),
    (translate "storage" storage contract.storage)

let contract = ref ""
let parameter = ref ""
let storage = ref ""

let translate () =
  let contract = FileString.read_file !contract in
  let parameter = !parameter in
  let storage = !storage in
  let p,s = data_of_liq ~contract ~parameter ~storage in
  List.iter (fun (s,x) ->
      match x with
      | None -> ()
      | Some x ->
         Printf.printf "%s: %s\n%!" s x)
            [ "parameter", p; "storage", s ]

end


let main () =
  let work_done = ref false in
  let arg_list = Arg.align [
      "-k", Arg.Set arg_keepon, " Continue on error";
      "--no-peephole", Arg.Clear arg_peephole,
      " Disable peephole optimizations";
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
  Arg.parse arg_list (fun s -> work_done := true; compile_file s)
            arg_usage;

  if not !work_done then
    Arg.usage arg_list arg_usage


let () =
  Printexc.record_backtrace true;
  try
    main ()
  with
  | Error (loc, msg) ->
    LiquidLoc.report_error (loc, msg);
    exit 2
(*
  | exn ->
     Printf.eprintf "Fatal Error: aborting\n";
     Printf.eprintf "  Exception: %s\n%!" (Printexc.to_string exn);
    exit 2
 *)
