(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open Lwt

exception RequestError of string

type from =
  | From_string of string
  | From_file of string

module type S = sig
  type 'a t
  val run : from -> string -> string -> (LiquidTypes.const * LiquidTypes.const) t
  val forge_deploy : from -> string list -> string t
  val deploy : from -> string list -> (string * string) t
  val get_storage : from -> string -> LiquidTypes.const t
  val forge_call : from -> string -> string -> string t
  val call : from -> string -> string -> string t
end

module Network_sync = struct
  let writer_callback a d =
    Buffer.add_string a d;
    String.length d

  let initialize_connection host path =
    let url = Printf.sprintf "%s%s" host path in
    let r = Buffer.create 16384
    and c = Curl.init () in
    Curl.set_timeout c 30;      (* Timeout *)
    Curl.set_sslverifypeer c false;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
    Curl.set_writefunction c (writer_callback r);
    Curl.set_tcpnodelay c true;
    Curl.set_verbose c false;
    Curl.set_post c false;
    Curl.set_url c url; r,c

  let post ?(content_type = "application/json") host path data =
    let r, c = initialize_connection host path in
    Curl.set_post c true;
    Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
    Curl.set_postfields c data;
    Curl.set_postfieldsize c (String.length data);
    Curl.perform c;
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    rc, (Buffer.contents r)
end

module Network = struct
  let writer_callback a d =
    Buffer.add_string a d;
    String.length d

  let initialize_connection host path =
    let url = Printf.sprintf "%s%s" host path in
    let r = Buffer.create 16384
    and c = Curl.init () in
    Curl.set_timeout c 30;      (* Timeout *)
    Curl.set_sslverifypeer c false;
    Curl.set_sslverifyhost c Curl.SSLVERIFYHOST_EXISTENCE;
    Curl.set_writefunction c (writer_callback r);
    Curl.set_tcpnodelay c true;
    Curl.set_verbose c false;
    Curl.set_post c false;
    Curl.set_url c url; r,c

  let post ?(content_type = "application/json") host path data =
    let r, c = initialize_connection host path in
    Curl.set_post c true;
    Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
    Curl.set_postfields c data;
    Curl.set_postfieldsize c (String.length data);
    Curl_lwt.perform c >>= fun cc ->
    if cc <> Curl.CURLE_OK then
      raise (RequestError
               (Printf.sprintf "[%d] [%s] Curl exception: %s\n%!"
                   (Curl.errno cc) host path))
    else
      let rc = Curl.get_responsecode c in
      Curl.cleanup c;
      Lwt.return (rc, (Buffer.contents r))
end

let curl_request ?(data="{}") path =
  let host = !LiquidOptions.tezos_node in
  if !LiquidOptions.verbosity > 0 then
    Printf.eprintf "\nRequest to %s%s:\n--------------\n%s\n%!"
      host path
      (* data; *)
      (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string data));
  try
    Network.post host path data >>= fun (status, json) ->
    if status <> 200 then
      raise (RequestError (
          Printf.sprintf "'%d' : curl failure %s%s" status host path)) ;
    if !LiquidOptions.verbosity > 0 then
      Printf.eprintf "\nNode Response %d:\n------------------\n%s\n%!"
        status
        (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string json));
    return json
  with Curl.CurlException (code, i, s) (* as exn *) ->
    raise (RequestError
                (Printf.sprintf "[%d] [%s] Curl exception: %s\n%!"
                   i host s))
(* raise exn *)

let request = ref curl_request


let compile_liquid liquid =
  let ocaml_ast, filename = match liquid with
    | From_string s ->
      LiquidFromOCaml.structure_of_string ~filename:"liquidity_buffer" s,
      "liquidity_buffer"
    | From_file f -> LiquidFromOCaml.read_file f, f
  in
  let syntax_ast, syntax_init, env =
    LiquidFromOCaml.translate ~filename ocaml_ast in
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true env syntax_ast in
  let encoded_ast, to_inline =
    LiquidEncode.encode_contract ~annot:!LiquidOptions.annotmic env typed_ast in
  let live_ast = LiquidSimplify.simplify_contract encoded_ast to_inline in
  let pre_michelson = LiquidMichelson.translate live_ast in
  let pre_michelson =
    if !LiquidOptions.peephole then
      LiquidPeephole.simplify pre_michelson
    else
      pre_michelson
  in
  let pre_init = match syntax_init with
    | None -> None
    | Some syntax_init ->
      let inputs_infos = fst syntax_init in
      Some (
        LiquidInit.compile_liquid_init env syntax_ast syntax_init,
        inputs_infos)
  in

  ( env, syntax_ast, pre_michelson, pre_init )

let raise_request_error r msg =
  try
    let err = Ezjsonm.find r ["error"] in
    (* let err_s = Ezjsonm.to_string ~minify:false (Data_encoding_ezjsonm.to_root err) in *)
    let l = Ezjsonm.get_list (fun err ->
        let kind = Ezjsonm.find err ["kind"] |> Ezjsonm.get_string in
        let id = Ezjsonm.find err ["id"] |> Ezjsonm.get_string in
        (* let err_s = Ezjsonm.to_string ~minify:false (Data_encoding_ezjsonm.to_root err) in *)
        kind ^ ": "^ id
      ) err in
    let err_s = "In "^msg^", "^ String.concat "\n" l in
    raise (RequestError err_s)
  with Not_found ->
    raise (RequestError ("Bad response for "^msg))


let mk_json_obj fields =
  fields
  |> List.map (fun (f,v) -> "\"" ^ f ^ "\":" ^ v)
  |> String.concat ","
  |> fun fs -> "{" ^ fs ^ "}"

let mk_json_arr l = "[" ^ String.concat "," l ^ "]"


let run_pre env syntax_contract pre_michelson source input storage =
  let c = LiquidToTezos.convert_contract ~expand:true pre_michelson in
  let input_m = LiquidToTezos.convert_const input in
  let storage_m = LiquidToTezos.convert_const storage in
  let contract_json = LiquidToTezos.json_of_contract c in
  let input_json = LiquidToTezos.json_of_const input_m in
  let storage_json = LiquidToTezos.json_of_const storage_m in
  let run_fields = [
      "script", contract_json;
      "input", input_json;
      "storage", storage_json;
      "amount", !LiquidOptions.amount;
  ] @ (match source with
      | None -> []
      | Some source -> ["contract", Printf.sprintf "%S" source]
    )
  in
  let run_json = mk_json_obj run_fields in
  !request ~data:run_json "/blocks/prevalidation/proto/helpers/run_code"
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let storage_r = Ezjsonm.find r ["ok"; "storage"] in
    let result_r = Ezjsonm.find r ["ok"; "output"] in
    let storage_expr = LiquidToTezos.const_of_ezjson storage_r in
    let result_expr = LiquidToTezos.const_of_ezjson result_r in
    let env = LiquidTezosTypes.empty_env env.filename in
    let storage =
      LiquidFromTezos.convert_const_type env storage_expr
        syntax_contract.storage
    in
    let result =
      LiquidFromTezos.convert_const_type env result_expr syntax_contract.return
    in
    return (result, storage)
  with Not_found ->
    raise_request_error r "run"


let run liquid input_string storage_string =
  let env, syntax_ast, pre_michelson, _ = compile_liquid liquid in
  let input =
    LiquidData.translate { env with filename = "run_input" }
      syntax_ast input_string syntax_ast.parameter
  in
  let storage =
    LiquidData.translate { env with filename = "run_storage" }
      syntax_ast storage_string syntax_ast.storage
  in
  run_pre env syntax_ast pre_michelson !LiquidOptions.source input storage


let get_counter source =
  !request
      (Printf.sprintf "/blocks/prevalidation/proto/context/contracts/%s/counter"
         source)
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["ok"] |> Ezjsonm.get_int |> return
  with Not_found ->
    raise_request_error r "get_counter"


let get_head_hash () =
  !request "/blocks/head" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["hash"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_request_error r "get_head_hash"

type head = {
  head_hash : string;
  head_netId : string;
}

let get_head () =
  !request "/blocks/head" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let head_hash = Ezjsonm.find r ["hash"] |> Ezjsonm.get_string in
    let head_netId = Ezjsonm.find r ["net_id"] |> Ezjsonm.get_string in
    return { head_hash; head_netId }
  with Not_found ->
    raise_request_error r "get_head"

let get_predecessor () =
  !request "/blocks/prevalidation/predecessor" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["predecessor"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_request_error r "get_predecessor"


let get_public_key_hash_from_secret_key sk =
    sk
    |> Sodium.Sign.secret_key_to_public_key
    |> Ed25519.Public_key.hash
    |> Ed25519.Public_key_hash.to_b58check


let forge_deploy ?head ?source liquid init_params_strings =
  let source = match source, !LiquidOptions.source with
    | Some source, _ | _, Some source -> source
    | None, None -> raise (RequestError "forge_deploy: Missing source")
  in
  let env, syntax_ast, pre_michelson, pre_init_infos = compile_liquid liquid in
  let pre_init, init_infos = match pre_init_infos with
    | None -> raise (RequestError "forge_deploy: Missing init")
    | Some pre_init_infos -> pre_init_infos
  in
  let init_storage_lwt = match pre_init with
    | LiquidInit.Init_constant c ->
      if init_params_strings <> [] then
        raise (RequestError "forge_deploy: Constant storage, no inputs needed");
      return c
    | LiquidInit.Init_code (syntax_c, c) ->
      let init_params =
        try
          List.map2 (fun input_str (input_name,_, input_ty) ->
            LiquidData.translate { env with filename = input_name }
              syntax_ast input_str input_ty
            ) init_params_strings init_infos
        with Invalid_argument _ ->
          raise
            (RequestError
               (Printf.sprintf
                  "forge_deploy: init storage needs %d arguments, but was given %d"
                  (List.length init_infos) (List.length init_params_strings)
               ))
      in
      let eval_init_storage = CUnit in
      let eval_init_input = match init_params with
        | [] -> CUnit
        | [x] -> x
        | _ -> CTuple init_params in

      run_pre env syntax_c c (Some source) eval_init_input eval_init_storage
      >>= fun (eval_init_result, _) ->
      Printf.eprintf "Evaluated initial storage: %s\n%!"
        (LiquidData.string_of_const eval_init_result);
      return (LiquidEncode.encode_const env syntax_ast eval_init_result)
  in
  init_storage_lwt >>= fun init_storage ->

  begin match head with
    | Some head -> return head
    | None -> get_head_hash ()
  end >>= fun head ->
  get_counter source >>= fun counter ->
  let counter = counter + 1 in
  let c = LiquidToTezos.convert_contract ~expand:true pre_michelson in
  let init_storage_m = LiquidToTezos.convert_const init_storage in
  let contract_json = LiquidToTezos.json_of_contract c in
  let init_storage_json = LiquidToTezos.json_of_const init_storage_m in

  let script_json = [
    "code", contract_json;
    "storage", init_storage_json
  ] |> mk_json_obj
  in
  let origination_json = [
    "kind", "\"origination\"";
    "managerPubkey", Printf.sprintf "%S" source;
    "balance", !LiquidOptions.amount;
    "spendable", "false";
    "delegatable", "false";
    "script", script_json;
  ] |> mk_json_obj
  in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "source", Printf.sprintf "%S" source;
    "fee", !LiquidOptions.fee;
    "counter", string_of_int counter;
    "operations", mk_json_arr [origination_json];
  ] |> mk_json_obj
  in
  !request ~data "/blocks/prevalidation/proto/helpers/forge/operations"
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["ok"; "operation"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_request_error r "forge_deploy"


let inject sk netId op =
  get_predecessor () >>= fun pred ->
  let op_b = MBytes.of_string (Hex_encode.hex_decode op) in
  let signature_b = Ed25519.sign sk op_b in
  let signature = Ed25519.Signature.to_b58check signature_b in
  let signed_op_b = MBytes.concat op_b signature_b in
  let signed_op = Hex_encode.hex_encode (MBytes.to_string signed_op_b) in
  let op_hash = Hash.Operation_hash.to_b58check @@
    Hash.Operation_hash.hash_bytes [ signed_op_b ] in
  let data = [
    "pred_block", Printf.sprintf "%S" pred;
    "operation_hash", Printf.sprintf "%S" op_hash;
    "forged_operation", Printf.sprintf "%S" op;
    "signature", Printf.sprintf "%S" signature;
  ] |> mk_json_obj
  in
  !request ~data "/blocks/prevalidation/proto/helpers/apply_operation"
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  (try
     Ezjsonm.find r ["ok"; "contracts"] |> Ezjsonm.get_list Ezjsonm.get_string
     |> return
   with Not_found ->
     raise_request_error r "inject (apply_operation)"
  ) >>= fun contracts ->
  let data = [
    "signedOperationContents", Printf.sprintf "%S" signed_op;
    "net_id", Printf.sprintf "%S" netId;
    "force", "true";
  ] |> mk_json_obj
  in
  !request ~data "/inject_operation" >>= fun r ->
  let r = Ezjsonm.from_string r in
  (try
     Ezjsonm.find r ["ok"; "injectedOperation"] |> Ezjsonm.get_string |> return
   with Not_found ->
     raise_request_error r "deploy (inject_operation)"
  ) >>= fun injected_op_hash ->
  assert (injected_op_hash = op_hash);

  return (injected_op_hash, contracts)


let deploy liquid init_params_strings =
  let sk = match !LiquidOptions.private_key with
    | None -> raise (RequestError "deploy: Missing private key")
    | Some sk -> match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ -> raise (RequestError "deploy: Bad private key")
  in
  let source = match !LiquidOptions.source with
    | Some source -> source
    | None -> get_public_key_hash_from_secret_key sk
  in
  get_head () >>= fun head ->
  forge_deploy ~head:head.head_hash ~source liquid init_params_strings
  >>= fun op ->
  inject sk head.head_netId op >>= function
  | op_h, [c] -> return (op_h, c)
  | _ -> raise (RequestError "deploy (inject)")


let get_storage liquid address =
  let env, syntax_ast, pre_michelson, pre_init_infos = compile_liquid liquid in
  !request
    (Printf.sprintf
       "/blocks/prevalidation/proto/context/contracts/%s/storage"
       address)
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let storage_r = Ezjsonm.find r ["ok"] in
    let storage_expr = LiquidToTezos.const_of_ezjson storage_r in
    let env = LiquidTezosTypes.empty_env env.filename in
    return
      (LiquidFromTezos.convert_const_type env storage_expr syntax_ast.storage)
  with Not_found ->
    raise_request_error r "get_storage"


let forge_call ?head ?source liquid address parameter_string =
  let source = match source, !LiquidOptions.source with
    | Some source, _ | _, Some source -> source
    | None, None -> raise (RequestError "forge_call: Missing source")
  in
  let env, syntax_ast, pre_michelson, pre_init_infos = compile_liquid liquid in
  let parameter =
    LiquidData.translate { env with filename = "call_parameter" }
      syntax_ast parameter_string syntax_ast.parameter
  in
  let parameter_m = LiquidToTezos.convert_const parameter in
  let parameter_json = LiquidToTezos.json_of_const parameter_m in
  begin match head with
    | Some head -> return head
    | None -> get_head_hash ()
  end >>= fun head ->
  get_counter source >>= fun counter ->
  let counter = counter + 1 in
  let transaction_json = [
    "kind", "\"transaction\"";
    "amount", !LiquidOptions.amount;
    "destination", Printf.sprintf "%S" address;
    "parameters", parameter_json;
  ] |> mk_json_obj
  in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "source", Printf.sprintf "%S" source;
    "fee", !LiquidOptions.fee;
    "counter", string_of_int counter;
    "operations", mk_json_arr [transaction_json];
  ] |> mk_json_obj
  in
  !request ~data "/blocks/prevalidation/proto/helpers/forge/operations"
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["ok"; "operation"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_request_error r "forge_call"


let call liquid address parameter_string =
  let sk = match !LiquidOptions.private_key with
    | None -> raise (RequestError "call: Missing private key")
    | Some sk -> match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ -> raise (RequestError "call: Bad private key")
  in
  let source = match !LiquidOptions.source with
    | Some source -> source
    | None -> get_public_key_hash_from_secret_key sk
  in
  get_head () >>= fun head ->
  forge_call ~head:head.head_hash ~source liquid address parameter_string
  >>= fun op ->
  inject sk head.head_netId op >>= function
  | op_h, [] -> return op_h
  | _ -> raise (RequestError "call (inject)")


(* Withoud optional argument head *)
module Async = struct
  type 'a t = 'a Lwt.t

  let forge_deploy liquid init_params_strings =
    forge_deploy liquid init_params_strings

  let forge_call liquid address parameter_string =
    forge_call liquid address parameter_string

  let run liquid input_string storage_string =
    run liquid input_string storage_string

  let deploy liquid init_params_strings =
    deploy liquid init_params_strings

  let get_storage liquid address =
    get_storage liquid address

  let call liquid address parameter_string =
    call liquid address parameter_string
end

module Sync = struct
  type 'a t = 'a

  let forge_deploy liquid init_params_strings =
    Lwt_main.run (forge_deploy liquid init_params_strings)

  let forge_call liquid address parameter_string =
    Lwt_main.run (forge_call liquid address parameter_string)

  let run liquid input_string storage_string =
    Lwt_main.run (run liquid input_string storage_string)

  let deploy liquid init_params_strings =
    Lwt_main.run (deploy liquid init_params_strings)

  let get_storage liquid address =
    Lwt_main.run (get_storage liquid address)

  let call liquid address parameter_string =
    Lwt_main.run (call liquid address parameter_string)

end
