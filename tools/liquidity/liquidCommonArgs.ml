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
  ]


let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S Manpage.s_bugs; `P "Report bugs at https://github.com/OCamlPro/liquidity/issues.";]
