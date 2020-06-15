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
open LiquidClientSigs
open Ezcmd.Modules

exception Bad_arg

module Data = struct

  let files = ref []
  let parameter = ref ""
  let storage = ref ""
  let entry_name = ref "default"
  let ty = ref ""
  let const = ref ""

  let contract_address = ref ""
  let init_inputs = ref []

  let get_files () = !files

  let get_inputs () = !init_inputs

  let validate_contract_addr s =
    if String.length s <> 36 ||
       (String.sub s 0 3 <> "KT1" &&
        let pref = String.sub s 0 2 in
        pref <> "tz" && pref <> "dn")
    then
      failwith (s ^ " is not a valid contract address")

  let validate_key_hash s =
    if String.length s <> 36 ||
       let pref = String.sub s 0 2 in
       pref <> "tz" && pref <> "dn" then
      failwith (s ^ " is not a valid key hash")

  let validate_private_key s =
    if ((String.length s <> 54 && String.length s <> 55) ||
        let p = String.sub s 0 4 in
        p <> "edsk" && p <> "spsk" && p <> "p2sk") then
      failwith (s ^ " is not a valid private key")

  let validate_public_key s =
    if ((String.length s <> 54 && String.length s <> 55) ||
        let p = String.sub s 0 4 in
        p <> "edpk" && p <> "sppk" && p <> "p2pk") then
      failwith (s ^ " is not a valid public key")
end


module type S = sig
  (* val report_err :
   *   ?kind:string -> Format.formatter -> LiquidTypes.location * string -> unit *)
  val report_error : exn -> unit
  val inject : string -> unit
  val run : unit -> unit
  val forge_deploy : unit -> unit
  val init_storage : unit -> unit
  val deploy : unit -> unit
  val get_storage : unit -> unit
  val call_arg : unit -> unit
  val call : unit -> unit
  val forge_call : unit -> unit
  val pack : unit -> unit
end

module Make (L: LANG with module Source = LiquidityLang ) : S = struct

  module Client = LiquidClient.Make(L)

  let report_err ?(kind="Error") fmt (err_loc, err_msg) =
    Format.fprintf fmt "%a: %s: @[%s@]\n%!" L.Source.print_loc err_loc kind err_msg

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
    | LiquidClientRequest.RequestError (code, msg) ->
      Format.eprintf "Request Error (code %d):\n%s@." code msg;
    | LiquidClientRequest.ResponseError msg ->
      Format.eprintf "Response Error:\n%s@." msg;
    | Client.E.RuntimeError (error, _trace) ->
      report_err ~kind:"Runtime error" Format.err_formatter error;
    | Client.E.LocalizedError error ->
      report_err ~kind:"Error" Format.err_formatter error;
    | Client.E.RuntimeFailure (error, None, _trace) ->
      report_err ~kind:"Failed at runtime" Format.err_formatter error;
    | Client.E.RuntimeFailure (error, Some v, _trace) ->
      report_err ~kind:"Failed at runtime" Format.err_formatter error;
      Format.eprintf "Failed with %s@." v#string;
    | Failure f ->
      Format.eprintf "Failure: %s@." f
    | Syntaxerr.Error (Syntaxerr.Other loc) ->
      report_err ~kind:"Syntax error" Format.err_formatter
        (LiquidLoc.loc_of_location loc, "unknown");
    | exn -> raise exn


  let inject file =
    let signature = match !LiquidOptions.signature with
      | None ->
        Printf.eprintf "Error: missing --signature option for --inject\n%!";
        exit 2
      | Some signature -> signature
    in
    (* an hexa encoded operation *)
    let operation = FileString.read_file file in
    let op_h = Client.Sync.inject
        ~operation:(Hex.to_bytes (`Hex operation))
        ~signature in
    Printf.printf "Operation injected: %s\n%!" op_h

  let get_contract () =
    From_files (Data.get_files ())
    |> L.Source.parse_contract
    |> L.Source.contract#ast

  let get_inputs () =
    Data.get_inputs ()
    |> List.map L.Source.const#string

  let run () =
    let open Client in
    let ops, r_storage, big_map_diff =
      Sync.run (get_contract ())
        !Data.entry_name
        (L.Source.const#string !Data.parameter)
        (L.Source.const#string !Data.storage)
    in
    Printf.printf "%s\n# Internal operations: %d\n%!"
      r_storage#string
      (List.length ops);
    match big_map_diff with
    | [] -> ()
    | diff ->
      let open Client.T in
      let open Client.T.Big_map_diff in
      Printf.printf "\nBig map diff:\n";
      let pp_id fmt = function
        | Bm_id id -> Format.fprintf fmt "[ID: %d]" id
        | Bm_name (id, name) -> Format.fprintf fmt "[%s (%d)]" name id in
      List.iter (fun item ->
          match item with
          | Big_map_add { id; key; value } ->
            Format.printf "%a +  %s --> %s\n" pp_id id key#string value#string
          | Big_map_remove { id; key } ->
            Format.printf "%a -  %s\n" pp_id id key#string
          | Big_map_delete { id } ->
            Format.printf "%a DELETE\n" pp_id id
          | Big_map_alloc { id } ->
            Format.printf "%a ALLOC\n" pp_id id
          | Big_map_copy { source_id; destination_id } ->
            Format.printf "%a COPY to %a\n" pp_id source_id pp_id destination_id
        ) diff;
      Printf.printf "%!"


  let forge_deploy () =
    let op =
      Client.Sync.forge_deploy (get_contract ()) (get_inputs ())
    in
    Printf.eprintf "Raw operation:\n--------------\n%!";
    Printf.printf "%s\n%!" Hex.(show @@ of_bytes op)

  let init_storage () =
    let outname =
      let c = match !LiquidOptions.main with
        | Some c -> c
        | None -> match List.rev (Data.get_files ()) with
          | c :: _ -> c
          | [] -> assert false in
      String.uncapitalize_ascii c in
    let storage =
      Client.Sync.init_storage (get_contract ()) (get_inputs ()) in
    if !LiquidOptions.json then
      let output = match !LiquidOptions.output with
        | Some output -> output
        | None -> outname ^ ".init.json" in
      FileString.write_file output
        (Ezjsonm.value_to_string ~minify:false storage#json);
      Printf.printf "Constant initial storage generated in %S\n%!" output
    else
      let output = match !LiquidOptions.output with
        | Some output -> output
        | None -> outname ^ ".init.tz" in
      FileString.write_file output storage#string;
      Printf.printf "Constant initial storage generated in %S\n%!" output

  let deploy () =
    let op_h, contract_id =
      Client.Sync.deploy (get_contract ()) (get_inputs ())
    in
    Printf.printf "New contract %s deployed in operation %s\n%!"
      contract_id op_h

  let get_storage () =
    let r_storage =
      Client.Sync.get_storage (get_contract ()) !Data.contract_address
    in
    Printf.printf "%s\n%!" r_storage#string

  let call_arg () =
    let arg = !Data.parameter |> Client.L.Source.const#string in
    let arg = Client.L.compile_const arg#ast |> Client.L.Target.const#ast in
    match !LiquidOptions.output with
    | None ->
      Printf.printf "Use --arg '%s'\n%!" arg#string
    | Some "-" ->
      Printf.printf "'%s'%!" arg#string
    | Some file ->
      FileString.write_file file arg#string

  let call () =
    let contract = match Data.get_files () with
      | [] -> None
      | l -> Some (get_contract ()) in
    let op_h =
      Client.Sync.call
        ?contract
        ~address:!Data.contract_address
        ~entry:!Data.entry_name
        (Client.L.Source.const#string !Data.parameter)
    in
    Printf.printf "Successful call to contract %s in operation %s\n%!"
      !Data.contract_address op_h

  let forge_call () =
    let contract = match Data.get_files () with
      | [] -> None
      | l -> Some (get_contract ()) in
    let op =
      Client.Sync.forge_call
        ?contract
        ~address:!Data.contract_address
        ~entry:!Data.entry_name
        (Client.L.Source.const#string !Data.parameter)
    in
    Printf.eprintf "Raw operation:\n--------------\n%!";
    Printf.printf "%s\n%!" Hex.(show @@ of_bytes op)

  let pack () =
    (match Data.get_files () with
     | [] -> ()
     | l ->
       (get_contract ())#ast
       |> Client.L.compile_contract
       |> ignore);
    let const = Client.L.Source.const#string !Data.const in
    let ty = Client.L.Source.datatype#string !Data.ty in
    let bytes = Client.Sync.pack ~const ~ty in
    Printf.printf "0x%s\n%!" Hex.(show @@ of_bytes bytes)

end


module MichelsonClient = Make(LiquidityToMichelson.Lang)
module LoveClient = Make(LiquidityToLove.Lang)

let client () = match !LiquidOptions.target_lang with
  | Michelson_lang -> (module MichelsonClient : S)
  | Love_lang -> (module LoveClient : S)

let parse_tez expl amount =
  match LiquidData.translate (LiquidFromParsetree.initial_env expl)
          dummy_contract_sig amount Ttez
  with
  | CTez t -> t
  | _ -> assert false

let docs = Manpage.s_options

let amount_arg =
  ["amount"; "a"],
  Arg.String (fun amount ->
      LiquidOptions.amount := parse_tez "--amount" amount
    ),
  Ezcmd.info ~docs ~docv:"<a>DUN"
    "Set amount to $(docv) for deploying or running a contract (default: 0DUN)"

let fee_arg =
  ["fee"; "f"],
  Arg.String (fun fee ->
      LiquidOptions.fee := Some (parse_tez "--fee" fee)
    ),
  Ezcmd.info ~docs ~docv:"<f>DUN"
    "Set fee $(docv) for deploying a contract (default: computed automatically)"

let gas_limit_arg =
  ["gas-limit"; "G"],
  Arg.Int (fun g -> LiquidOptions.gas_limit := Some g),
  Ezcmd.info ~docs ~docv:"g"
    "Set gas limit to $(docv) (default: automatic)"

let storage_limit_arg =
  ["storage-limit"; "S"],
  Arg.Int (fun s -> LiquidOptions.storage_limit := Some s),
  Ezcmd.info ~docs ~docv:"s"
    "Set storage limit to $(docv) (default: automatic)"

let source_arg =
  ["source"; "s"], Arg.String (fun s ->
      Data.validate_key_hash s;
      LiquidOptions.source := Some s),
  Ezcmd.info ~docs ~docv:"dn1..."
    "Set the source to $(docv) for manager operations \
     (default: derived from public key)"

let private_key_arg =
  ["private-key"], Arg.String (fun s ->
      Data.validate_private_key s;
      LiquidOptions.private_key := Some s),
  Ezcmd.info ~docs ~docv:"edsk..."
    "Set the private key to $(docv) for signed operations (default: none)"

let public_key_arg =
  ["public-key"], Arg.String (fun s ->
      Data.validate_public_key s;
      LiquidOptions.public_key := Some s),
  Ezcmd.info ~docs ~docv:"edpk..."
    "Set the public key to $(docv) for revelations \
     (default: derived from private key)"

let counter_arg =
  ["counter"; "C"], Arg.Int (fun n -> LiquidOptions.counter := Some n),
  Ezcmd.info ~docs ~docv:"N"
    "Set the counter to $(docv) for the operation instead of retrieving it"

let node_arg =
  ["node"; "n"], Arg.Set_string LiquidOptions.node,
  Ezcmd.info ~docs ~docv:"ADDR:PORT"
    "Set the address and port to $(docv) of a node to run or deploy \
     contracts (default: 127.0.0.1:8733)"

let common_args = LiquidCommonArgs.common @ [
    ["files"],
    Arg.File (fun s -> let l = String.split_on_char ',' s in Data.files := l),
    Ezcmd.info ~docs:Manpage.s_common_options ~docv:"FILE1,FILE2,..."
      "Filenames to compile (comma separated)";
  ]

let arg_entry i s =
  [],
  Arg.Anon (i, fun s -> Data.entry_name := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"ENTRY"
    ("Entry point name " ^ s)

let arg_parameter i s =
  [],
  Arg.Anon (i, fun s -> Data.parameter := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"PARAMETER"
    ("Parameter value " ^ s)

let arg_storage i s =
  [],
  Arg.Anon (i, fun s -> Data.storage := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"STORAGE"
    ("Storage value " ^ s)

let arg_type i s =
  [],
  Arg.Anon (i, fun s -> Data.ty := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"TYPE"
    ("Type " ^ s)

let arg_constant i s =
  [],
  Arg.Anon (i, fun s -> Data.const := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"CONSTANT"
    ("Constant value " ^ s)

let arg_deploy_inputs =
  [],
  Arg.Anons (fun s -> Data.init_inputs := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"INPUTS"
    ("Inputs to initializtion function")


let arg_address i =
  [],
  Arg.Anon (i, fun s ->
      Data.validate_contract_addr s;
      Data.contract_address := s),
  Ezcmd.info ~docs:Manpage.s_options ~docv:"ADDRESS"
    ("KT1... address of the contract")


let run_cmd =
  {
    Arg.cmd_name = "run";
    cmd_args = common_args @ [
        arg_entry 0 "for simulation";
        arg_parameter 1 "for simulation";
        arg_storage 2 "for simulation";
        source_arg;
        public_key_arg;
        node_arg;
        amount_arg;
      ];
    cmd_doc = "Run (simulate) a transation on a Liquidity contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Run (simulate) a transation on a Liquidity contract";

      `S Manpage.s_examples;
      `Pre {|
liquidity-client run \\
  --files tests/others/multisig.liq \\
  --node http://testnet-node.dunscan.io \\
  --source dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb  \\
  manage 'Some { destination = dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8; amount = 10DUN }' '{owners = (Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]); actions = Map; owners_length = 1p; min_agree = 1p}'


{
  owners = (Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]);
  actions = Map;
  owners_length = 1p;
  min_agree = 1p
}
# Internal operations: 1
|};

      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.run ();
  }

let init_storage_cmd =
  {
    Arg.cmd_name = "init-storage";
    cmd_args = common_args @ [
        arg_deploy_inputs;
        source_arg;
        node_arg;
      ];
    cmd_doc = "Generate input storage";
    cmd_man = [
      `S Manpage.s_description;
      `P "Generate input storage with initialization function";
      `S Manpage.s_examples;
      `P "Failing example:";`Noblank;
      `Pre {|
liquidity-client init-storage --files tests/others/multisig.liq  --node http://testnet-node.dunscan.io 'Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]' 2p


tests/others/multisig.liq:25.4-25.74: Failed at runtime: in /chains/main/blocks/head/helpers/scripts/run_code
Failed with "Number of owners must be greater or equal to quorum"
|};
      `P "Working example:";`Noblank;
      `Pre {|
./liquidity-client init-storage --files tests/others/multisig.liq  --node http://testnet-node.dunscan.io 'Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]' 1p

Constant initial storage generated in "tests/others/multisig.liq.init.tz"
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.init_storage ();
  }


let forge_deploy_cmd =
  {
    Arg.cmd_name = "forge-deploy";
    cmd_args = common_args @ [
        arg_deploy_inputs;
        source_arg;
        public_key_arg;
        node_arg;
        counter_arg;
        amount_arg;
        fee_arg; gas_limit_arg; storage_limit_arg;
      ];
    cmd_doc = "Forge deployment operation for contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Forge unsigned deployment operation for contract";

      `S Manpage.s_examples;
      `Pre {|
liquidity-client forge-deploy --files tests/others/multisig.liq  --node http://testnet-node.dunscan.io --source dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb 'Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]' 1p


Raw operation:
--------------
9649d986044c498fc5e526a0c9608c79b73103ced669aed64d45445dd351b9946d0011589aef8...
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.forge_deploy ();
  }

let deploy_cmd =
  {
    Arg.cmd_name = "deploy";
    cmd_args = common_args @ [
        arg_deploy_inputs;
        source_arg;
        node_arg;
        counter_arg;
        public_key_arg;
        private_key_arg;
        amount_arg;
        fee_arg; gas_limit_arg; storage_limit_arg;
      ];
    cmd_doc = "Deploy contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Deploy contract on a network";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client deploy --files tests/others/multisig.liq  --node http://testnet-node.dunscan.io  --private-key edsk3impUREDjtAvDvz8MWQsY7JQyFHhBfVoU6CztBUCLw3ocuqqJ7 'Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]' 1p


New contract KT1Lb7UdXq88n2ahpjwXfbrQPkgjyKu6BoC2 deployed in operation oovkxqLCSk88RYgQ8e22aW4dy1ba7DXvHQTdFYPJg58JWun6qn3
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.deploy ();
  }

let get_storage_cmd =
  {
    Arg.cmd_name = "get-storage";
    cmd_args = common_args @ [
        arg_address 0;
        node_arg;
      ];
    cmd_doc = "Retrieve storage value for a contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Retrieve storage value for a contract";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client get-storage --files tests/others/multisig.liq --node http://testnet-node.dunscan.io KT1Lb7UdXq88n2ahpjwXfbrQPkgjyKu6BoC2


{
  owners = (Set [dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb]);
  actions = Map;
  owners_length = 1p;
  min_agree = 1p
}
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.get_storage ();
  }


let call_cmd =
  {
    Arg.cmd_name = "call";
    cmd_args = common_args @ [
        arg_address 0;
        arg_entry 1 "for call";
        arg_parameter 2 "for call";
        source_arg;
        node_arg;
        counter_arg;
        public_key_arg;
        private_key_arg;
        amount_arg;
        fee_arg; gas_limit_arg; storage_limit_arg;
      ];
    cmd_doc = "Call a smart contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Call a deployed smart contract";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client call --files tests/others/multisig.liq --node http://testnet-node.dunscan.io  --private-key edsk3impUREDjtAvDvz8MWQsY7JQyFHhBfVoU6CztBUCLw3ocuqqJ7 KT1Lb7UdXq88n2ahpjwXfbrQPkgjyKu6BoC2 pay '()'

Successful call to contract KT1Lb7UdXq88n2ahpjwXfbrQPkgjyKu6BoC2 in operation oozWUtgrnTNgfY5jG1NNdmHosYi3vKNMGKr2ZfzRZM8ReEkddNV
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.call ();
  }

let forge_call_cmd =
  {
    Arg.cmd_name = "forge-call";
    cmd_args = [
        arg_address 0;
        arg_entry 1 "for call";
        arg_parameter 2 "for call";
        source_arg;
        public_key_arg;
        node_arg;
        counter_arg;
        amount_arg;
        fee_arg; gas_limit_arg; storage_limit_arg;
      ] @ common_args;
    cmd_doc = "Forge a call operation to a smart contract";
    cmd_man = [
      `S Manpage.s_description;
      `P "Forge an unsigned call operation to a smart contract";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client forge-call --node http://testnet-node.dunscan.io --source dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb KT1Lb7UdXq88n2ahpjwXfbrQPkgjyKu6BoC2 pay '()' --amount 10DUN


Raw operation:
--------------
50d636cc2440f9042c4e79bb29c67ce6488aa1f5b419d4abbd22fc8ff8f4e1ab6c0011589aef8b9cd48925f6fedadcee774d51d14b85912e8b0bd3b8030080ade2040183b221732a1f1b05d442a81967ae8188ed44b6c300ffff0370617900000002030b|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.forge_call ();
  }

let call_arg_cmd =
  {
    Arg.cmd_name = "call-arg";
    cmd_args = common_args @ [
        arg_parameter 0 "for call";
      ];
    cmd_doc = "Compile a parameter for a contract call call";
    cmd_man = [
      `S Manpage.s_description;
      `P "Compile a parameter for a contract call call";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client call-arg '()'


Use --arg 'Unit'
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.call_arg ();
  }

let pack_cmd =
  {
    Arg.cmd_name = "pack";
    cmd_args = common_args @ [
        arg_constant 0 "for pack";
        arg_type 1 "of value to be packed (serialized)";
        node_arg;
      ];
    cmd_doc = "Serialize (pack) a constant value";
    cmd_man = [
      `S Manpage.s_description;
      `P "Serialize (pack) a constant value";
      `S Manpage.s_examples;
      `Pre {|
liquidity-client pack --node http://testnet-node.dunscan.io  '()' unit

0x05030b
|};
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.pack ();
  }

let inject_cmd =
  let opf = ref "" in
  {
    Arg.cmd_name = "inject";
    cmd_args = common_args @ [
        ["signature"],
        Arg.String (fun s -> LiquidOptions.signature := Some s),
        Ezcmd.info ~docs:Manpage.s_options ~docv:"edsig..."
          "Set the signature for an operation";

        node_arg;

        [],
        Arg.Anon (0, fun s -> opf := s),
        Ezcmd.info ~docs:Manpage.s_options ~docv:"OPERATION.bytes"
          "File containing the unsigned operation";
      ];
    cmd_doc = "Inject a sign operation";
    cmd_man = [
      `S Manpage.s_description;
      `P "Inject a sign operation";
      `Blocks LiquidCommonArgs.help_secs;
    ];
    cmd_action = fun () ->
      let module Client = (val client ()) in
      Client.inject !opf;
  }

let main () =
  let name = "liquidity-client" in
  let doc = "a client for Dune Network and Tezos to inteact with Liquidity \
             smart contracts" in
  let man = LiquidCommonArgs.help_secs in
  Ezcmd.main_with_subcommands
    ~name
    ~doc
    ~man
    (* ~default:compile_cmd *)
    [
      run_cmd;
      inject_cmd;
      pack_cmd;
      call_arg_cmd;
      forge_call_cmd;
      call_cmd;
      get_storage_cmd;
      deploy_cmd;
      forge_deploy_cmd;
      init_storage_cmd;
    ]


let () =
  Printexc.record_backtrace true;
  try
    main ()
  with exn ->
    (try MichelsonClient.report_error exn
     with exn ->
     try LoveClient.report_error exn
     with exn -> let backtrace = Printexc.get_backtrace () in
       Format.eprintf "Error: %s\nBacktrace:\n%s@."
         (Printexc.to_string exn) backtrace
    );
    exit 1
