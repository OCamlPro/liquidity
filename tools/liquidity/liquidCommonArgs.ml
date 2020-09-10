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

open Ezcmd.Modules

let network_env =
  let docs = Manpage.s_environment in
  Ezcmd.env ~docs "LIQUID_NETWORK"
    ~doc:"Network to use, possible values: $(i,Dune), $(i,Tezos)."

let common =
  let docs = Manpage.s_common_options in
  [
    ["verbose"; "v"],
    Arg.Unit (fun () -> incr LiquidOptions.verbosity),
    Ezcmd.info ~docs "Increment verbosity";

    ["V"],
    Arg.Set_int LiquidOptions.verbosity,
    Ezcmd.info ~docs ~docv:"level" "Verbosity level";

    ["reason"; "re"],
    Arg.Clear LiquidOptions.ocaml_syntax,
    Ezcmd.info ~docs "Use ReasonML syntax";

    ["target"; "t"],
    Arg.Symbol (["michelson"; "love"; "Michelson"; "Love"],
                function
                | "michelson" | "Michelson" ->
                  LiquidOptions.target_lang := Michelson_lang
                | "love" | "Love"  ->
                  LiquidOptions.target_lang := Love_lang
                | s ->
                  Format.eprintf "%s not allowed for target@." s;
                  exit 1),
    Ezcmd.info ~docs ~docv:"LANG"
      "Set the target language (possible values: $(i,michelson), $(i,love)).";

    ["version"],
    Arg.Unit (fun () ->
        Format.printf "%s" LiquidVersion.version;
        if !LiquidOptions.verbosity > 0 then
          Format.printf " (%s)" LiquidVersion.commit;
        if !LiquidOptions.verbosity > 1 then
          Format.printf "\nCompiled on %s" LiquidVersion.en_date;
        Format.printf "@.";
        exit 0
      ),
    Ezcmd.info ~docs "Show version and exit";

    ["network"; "N"],
    Arg.Symbol (["dune"; "Dune"; "DUNE"; "tezos"; "Tezos"; "TEZOS"],
                function
                | "dune" | "Dune" | "DUNE" ->
                  LiquidOptions.network := Dune_network;
                | "tezos" | "Tezos" | "TEZOS" ->
                  LiquidOptions.network := Tezos_network;
                | s ->
                  Format.eprintf "%s not allowed for network@." s;
                  exit 1
               ),
    Ezcmd.info ~docs ~docv:"NETWORK" ~env:network_env
      "Set the network to use (possible values: $(i,Dune), $(i,Tezos)).";

    ["main"; "m"],
    Arg.String (fun main -> LiquidOptions.main := Some main),
    Ezcmd.info ~docs ~docv:"ContractName"
      "Produce code for contract named $(docv)";

    ["no-inline"],
    Arg.Clear LiquidOptions.inline,
    Ezcmd.info ~docs "Disable inlining";

    ["no-simplify"],
    Arg.Clear LiquidOptions.simplify,
    Ezcmd.info ~docs "Disable simplifications";

    ["no-peephole"],
    Arg.Clear LiquidOptions.peephole,
    Ezcmd.info ~docs "Disable peephole optimizations";

    ["compact"; "c"],
    Arg.Set LiquidOptions.singleline,
    Ezcmd.info ~docs "Produce compact Michelson";

    ["no-annot"],
    Arg.Set LiquidOptions.no_annot,
    Ezcmd.info ~docs "Don't produce any annotations when compiling";

    ["no-ignore-annots"],
    Arg.Clear LiquidOptions.retry_without_annots,
    Ezcmd.info ~docs "Don't ignore annotations of failure when decompiling";

    ["no-uncurry"],
    Arg.Set LiquidOptions.no_uncurrying,
    Ezcmd.info ~docs "Don't uncurry non partially applied lambdas";

    ["no-love-typecheck"],
    Arg.Set LiquidOptions.no_love_typecheck,
    Ezcmd.info ~docs "Don't typecheck resulting Love output";
  ]


let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S Manpage.s_bugs; `P "Report bugs at https://github.com/OCamlPro/liquidity/issues.";]
