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

open LiquidTypes

open Tezos_protocol.Protocol
open Environment
open Love_pervasives

module DebugPrint = LiquidPrinter.LiquidDebug

exception Bad_arg

(* We use the parser of the OCaml compiler parser to parse the file,
   we then translate it to a simplified AST, before compiling it
   to Michelson. No type-checking yet.
*)

let compile_liquid_files filename =
  let ocaml_ast = LiquidFromParsetree.read_file filename in
  let syntax_ast = LiquidFromParsetree.translate ~filename ocaml_ast in
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true ~decompiling:false ~monomorphise:true ~keep_tvars:true syntax_ast in
  let typed_ast_no_tfail = Preprocess.contract_ttfail_to_tvar typed_ast in
  let ctr_name = Compil_utils.get_ctr_name filename in
  let love_ast, _ = Liq2love.liqcontract_to_lovecontract ~ctr_name typed_ast_no_tfail in
  Log.debug "Love contract : %a" Love_printer.Ast.print_structure love_ast;
(*
  let tenv =
      (Love_tenv.empty (Contract []) ()) in    
  let new_c,tenv =
    Love_typechecker.typecheck_struct
      None
      tenv
      love_ast in
  Log.debug
    "*********Final environment**********\n%a@."
    Love_tenv.pp_env tenv;
  Log.debug "Love program : %a"
    Love_printer.Ast.print_structure new_c; *)
  let () =
    let json =
      let str = {Love_ast.version = 1, 0; code = love_ast} in
      Environment.Data_encoding.Json.construct Love_json_encoding.Ast.top_contract_encoding str
    in
    let love_program =
      Format.asprintf
        "#love-json\n\
         \n\
         %s"
        (Environment.Data_encoding.Json.to_string json)
    in
    Format.printf "Writing file %s@." (filename ^ ".json");
    FileString.write_file (filename ^ ".json")
      love_program
  in
  let () =
    let love_program =
      Format.asprintf
        "#love\n\
         \n\
         %a"
        Love_printer.Ast.print_structure love_ast
    in
    Format.printf "Writing file %s@." (filename ^ ".lov");
    FileString.write_file (filename ^ ".lov")
      love_program
  in
  ()

module Data = struct
  let files = ref []
  let parameter = ref 0
  let amount = ref 0
  let storage = ref ""
  let entry_name = ref "main"
end

let main () =
  Log.fmt := (fun _ -> Format.std_formatter);
  let arg_list = Arg.align [
      "--version",
      Arg.Unit (fun () -> Format.printf "MakeLove / 0.1"; exit 0),
      " Show version and exit";
      "--debug",
      Arg.Bool (fun b -> Options.debug := b;),
      " Activates debug messages";
    ]
  in
  let arg_usage = String.concat "\n" [
      "love [OPTIONS] FILE [COMMAND]";
      "";
      "The love tool allows to typecheck and simulate Love files before";
      "deployment to the Dune Network. Love files must end with the";
      "the .love extension.";
      "";
      "Available options:";
    ]
  in
  let () = 
    Format.printf "Initialisation environments@.";
    Love_type_list.init ();
    Love_prim_list.init ();
    Love_tenv.init_core_env ();
    Format.printf "Initialisation complete@.";
  in
  Format.printf "...Compiling Liquidity file@.";
  Arg.parse arg_list (fun s -> Data.files := s :: !Data.files) arg_usage;
  if !Data.files = [] || List.length !Data.files > 1 then raise Bad_arg;
  let file = List.hd !Data.files in
  compile_liquid_files file;
  Format.printf "Done.@."

let () = main ()
