(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
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
open Ezcmd.Modules
module DebugPrint = LiquidPrinter.LiquidDebug

open Dune_Network_Lib
open Protocol
open Environment
open Love_pervasives

(* We use the parser of the OCaml compiler parser to parse the file,
   we then translate it to a simplified AST, before compiling it
   to Michelson. No type-checking yet.
*)

let typecheck_liquid_files ?monomorphise ?keep_tvars files =
  let ocaml_asts = List.map (fun filename ->
      let ocaml_ast = LiquidFromParsetree.read_file filename in
      (* Format.eprintf "%s\n================\n@."
       *   (LiquidPrinter.Syntax.string_of_structure ocaml_ast []); *)
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
      ~warnings:true ~decompiling:false ?monomorphise ?keep_tvars syntax_ast in
  typed_ast, outprefix

let output_final ~to_json ~to_readable ~outprefix ~ext c =
  if !LiquidOptions.json then
    let s = to_json c in
    let output = match !LiquidOptions.output with
      | Some output -> output
      | None -> String.concat "." [outprefix; ext; "json"] in
    match output with
    | "-" -> Printf.printf "%s\n%!" s
    | _ ->
      FileString.write_file output s;
      Printf.eprintf "File %S generated\n%!" output;
      Printf.eprintf "If you have a node running, \
                      you may want to typecheck with:\n";
      Printf.eprintf "  curl http://127.0.0.1:8733/chains/main/blocks/head/\
                      helpers/scripts/typecheck_code -H \
                      \"Content-Type:application/json\" \
                      -d '{\"program\":'$(cat %s)'}'\n" output
  else
    let s = to_readable c in
    match
      match !LiquidOptions.output with
      | Some output -> output
      | None -> String.concat "." [outprefix; ext]
    with
    | "-" -> Printf.printf "%s%!" s
    | output ->
      FileString.write_file output s;
      Printf.eprintf "File %S generated\n%!" output;
      Printf.eprintf "You may want to typecheck with:\n";
      Printf.eprintf "  %s-client typecheck script %s\n"
        (String.lowercase_ascii (LiquidOptions.network_name ()))
        output

let compile_liquid_files_to_michelson files =
  let typed_ast, outprefix = typecheck_liquid_files files in
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
        else StringMap.empty, StringMap.empty in
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
              (full_sig_of_contract typed_ast) init with
      | LiquidInit.Init_constant c_init when !LiquidOptions.json ->
        let c_init = LiquidMichelson.compile_const c_init in
        let s = LiquidToMicheline.(json_of_const @@ convert_const ~expand:true c_init) in
        let output = outprefix ^ ".init.json" in
        FileString.write_file output s;
        Printf.eprintf "Constant initial storage generated in %S\n%!" output
      | LiquidInit.Init_constant c_init ->
        let c_init = LiquidMichelson.compile_const c_init in
        let s = LiquidToMicheline.(string_of_const @@ convert_const ~expand:false c_init) in
        let output = outprefix ^ ".init.tz" in
        FileString.write_file output s;
        Printf.eprintf "Constant initial storage generated in %S\n%!" output
      | LiquidInit.Init_code init_sim ->
        let pre_init = LiquidMichelson.translate init_sim in
        let mic_init, _ = LiquidToMicheline.convert_contract ~expand:true pre_init in
        let s, output =
          if !LiquidOptions.json then
            LiquidToMicheline.json_of_contract mic_init,
            outprefix ^ ".initializer.tz.json"
          else
            LiquidToMicheline.string_of_contract mic_init,
            outprefix ^ ".initializer.tz"
        in
        FileString.write_file output s;
        Printf.eprintf "Storage initializer generated in %S\n%!" output
  end;

  let c, loc_table =
    LiquidToMicheline.convert_contract ~expand:(!LiquidOptions.json) pre_michelson
  in

  output_final
    ~to_json:LiquidToMicheline.json_of_contract
    ~to_readable:(fun c ->
        let s =
          if !LiquidOptions.singleline
          then LiquidToMicheline.line_of_contract c
          else LiquidToMicheline.string_of_contract c in
        if !LiquidOptions.writeinfo then
          LiquidInfomark.gen_info
            ~decompile:false ~language:"Michelson" files ^ s
        else s)
    ~outprefix
    ~ext:"tz"
    c

let compile_liquid_files_to_love files =
  let typed_ast, outprefix =
    typecheck_liquid_files ~monomorphise:true ~keep_tvars:true files in
  let typed_ast_no_tfail = Preprocess.contract_ttfail_to_tvar typed_ast in
  let ctr_name = typed_ast.contract_name in
  let love_ast, _ = Liq2love.liqcontract_to_lovecontract ~ctr_name typed_ast_no_tfail in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (outprefix ^ ".love_ast")
      (Format.asprintf "%a" Love_printer.Ast.print_structure love_ast);
  let to_json love_ast = Liq2love.print_contract_json love_ast in
  let to_readable love_ast =
    let info =
      if !LiquidOptions.writeinfo then
        LiquidInfomark.gen_info
          ~decompile:false ~language:"Love" files
      else "" in
    String.concat "" [
      "#love\n\n";
      info;
      Format.asprintf "%a" Love_printer.Ast.print_structure love_ast
    ] in
  output_final ~to_json ~to_readable ~outprefix ~ext:"lov" love_ast


let compile_tezos_file filename =
  let code, env =
    if Filename.check_suffix filename ".json" || !LiquidOptions.json then
      LiquidToMicheline.read_micheline_json filename
    else LiquidToMicheline.read_micheline_file filename
  in
  let c = LiquidFromMicheline.convert_contract env code in
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
      LiquidCheck.typecheck_contract
        ~keep_tvars:true ~warnings:false ~decompiling:true c1
    with LiquidError _ ->
      (* Retry with generalization of types *)
      LiquidMichelineTypes.set_generalize_types env true;
      ignore (LiquidFromMicheline.convert_contract env code);
      (* for side effects in generalized type definitions *)
      let c2 = LiquidDecomp.decompile env c in
      if !LiquidOptions.verbosity>0 then
        FileString.write_file  (filename ^ ".pre")
          (DebugPrint.string_of_contract c2);
      LiquidCheck.typecheck_contract
        ~keep_tvars:true ~warnings:false ~decompiling:true c2
  in
  let annoted_tz, type_annots, types = LiquidFromMicheline.infos_env env in
  let encode_ast, to_inline =
    LiquidEncode.encode_contract ~decompiling:true typed_ast in
  let live_ast = LiquidSimplify.simplify_contract
      ~decompile_annoted:annoted_tz encode_ast to_inline in
  if !LiquidOptions.verbosity>0 then
    FileString.write_file (filename ^ ".simple")
      (DebugPrint.string_of_contract
         live_ast);
  let multi_ast = LiquidDecode.decode_contract live_ast in
  let untyped_ast = LiquidUntype.untype_contract multi_ast in
  (* let untyped_ast = c in *)
  let output = match !LiquidOptions.output with
    | Some output -> output
    | None -> outprefix ^
              if !LiquidOptions.ocaml_syntax then ".liq" else ".reliq" in
  let s =
    LiquidCheck.typecheck_contract ~warnings:false ~decompiling:true untyped_ast
    |> LiquidToParsetree.structure_of_contract ~type_annots ~types
    |> fun s -> LiquidPrinter.Syntax.string_of_structure s []
  in
  let s =
    if !LiquidOptions.writeinfo then
      LiquidInfomark.gen_info
        ~decompile:true ~language:"Liquidity"
        [filename] ^ s ^ "\n"
    else s ^ "\n" in
  match output with
  | "-" ->
    Format.printf "%s%!" s
  | _ ->
    FileString.write_file output s;
    Printf.eprintf "File %S generated\n%!" output;
    ()

let report_err ?(kind="Error") fmt (err_loc, err_msg) =
  Format.fprintf fmt "%a: %s: @[%s@]\n%!" LiquidLoc.print_loc err_loc kind err_msg

let report_error = function
  | LiquidError error ->
    report_err Format.err_formatter (error.err_loc, error.err_msg);
  | LiquidNamespace.Unknown_namespace (p, err_loc) as exn ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf "Error: %s\nBacktrace:\n%s@."
      (Printexc.to_string exn) backtrace ;
    report_err Format.err_formatter
      (err_loc,
       Printf.sprintf "Unknown module or contract %s" (String.concat "." p));
  | LiquidFromMicheline.Missing_program_field f ->
    Format.eprintf "Missing script field %s@." f;
  | Failure f ->
    Format.eprintf "Failure: %s@." f
  | Syntaxerr.Error (Syntaxerr.Other loc) ->
    report_err ~kind:"Syntax error" Format.err_formatter
      (LiquidLoc.loc_of_location loc, "unknown");
  | exn ->
    let backtrace = Printexc.get_backtrace () in
    Format.eprintf "Error: %s\nBacktrace:\n%s@."
      (Printexc.to_string exn) backtrace

let compile_tezos_file filename =
  try compile_tezos_file filename
  with exn ->
    (* Rety and ignore annotations if failing *)
    if not !LiquidOptions.retry_without_annots ||
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


let compile_liquid_files files =
  begin match !LiquidOptions.output with
    | None -> ()
    | Some o ->
      if Filename.check_suffix o ".tz" then
        LiquidOptions.target_lang := Michelson_lang;
      if Filename.check_suffix o ".lov" then
        LiquidOptions.target_lang := Love_lang;
  end;
  match !LiquidOptions.target_lang with
  | Michelson_lang -> compile_liquid_files_to_michelson files
  | Love_lang -> compile_liquid_files_to_love files

module Data = struct
  let files = ref []
  let get_files () = !files
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
  if List.exists (fun filename ->
      Filename.check_suffix filename ".lov") unknown then begin
    Format.eprintf "Cannot decompile Love files yet: %a.@."
      (Format.pp_print_list Format.pp_print_string) unknown;
    exit 2
  end;
  match unknown with
    [] ->
    begin match liq_files, tz_files with
      | [], [] ->
        Format.eprintf "Error: No files given as arguments@.";
        exit 1
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
  match !LiquidOptions.output with
  | None | Some "-" ->
    Format.printf "%s%!" s
  | Some output ->
    FileString.write_file output s;
    Printf.eprintf "File %S generated\n%!" output;
    ()

let docs = Manpage.s_common_options

let common_args =
  LiquidCommonArgs.common @ [
    ["o"; "output"],
    Arg.String (fun o -> LiquidOptions.output := Some o),
    Ezcmd.info ~docs:Manpage.s_options ~docv:"filename" "Output code in $(docv)";

    ["json"; "j"],
    Arg.Set LiquidOptions.json,
    Ezcmd.info ~docs "Output in JSON representation";

    ["type-only"],
    Arg.Set LiquidOptions.typeonly,
    Ezcmd.info ~docs "Stop after type checking";

    ["parse-only"],
    Arg.Set LiquidOptions.parseonly,
    Ezcmd.info ~docs "Stop after parsing";

  ]

let main () =
  let name = "liquidity" in
  let doc = "a compiler for the smart contract Language Liquidity \
             for Dune Network and Tezos" in
  let man = [
    `S Manpage.s_description;
    `P "Compile Liquidity files to Michelson, or decompile a Michelson file to \
        a Liquidity one.";
    `Blocks LiquidCommonArgs.help_secs;
  ] in
  Ezcmd.main {
    Arg.cmd_name = name;
    cmd_args = common_args @ [
        ["convert"],
        Arg.String (fun f -> convert_file f; exit 0),
        Ezcmd.info ~docs:Manpage.s_options ~docv:"filename"
          "Convert $(docv) to Liquidity or ReasonML syntax";

        [],
        Arg.Anons (fun s -> Data.files := s),
        Ezcmd.info ~docs:Manpage.s_options ~docv:"FILES" "Filenames to compile";
      ];
    cmd_doc = doc;
    cmd_man = man;
    cmd_action = compile_files;
  }


let () =
  Printexc.record_backtrace true;
  try
    main ()
  with exn ->
    report_error exn;
    exit 1
