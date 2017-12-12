(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


module Network = struct
  open Curl
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


exception RequestError of string

let request ?(data="{}") path =
  let host = !LiquidOptions.tezos_node in
  if !LiquidOptions.verbosity > 0 then
    Printf.eprintf "\nRequest to %s%s:\n--------------\n%s\n%!"
      host path
      (* data; *)
      (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string data));
  try
    let (status, json) = Network.post host path data in
    if status <> 200 then
      raise (RequestError (
          Printf.sprintf "'%d' : curl failure %s%s" status host path)) ;
    if !LiquidOptions.verbosity > 0 then
      Printf.eprintf "\nNode Response %d:\n------------------\n%s\n%!"
        status
        (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string json));
    json
  with Curl.CurlException (code, i, s) (* as exn *) ->
    raise (RequestError
             (Printf.sprintf "[%d] [%s] Curl exception: %s\n%!"
                i host s))
    (* raise exn *)


type from =
  | From_string of string
  | From_file of string

let compile_liquid liquid =
  let ocaml_ast = match liquid with
    | From_string s -> LiquidFromOCaml.structure_of_string s
    | From_file f -> LiquidFromOCaml.read_file f
  in
  let syntax_ast, syntax_init, env =
    LiquidFromOCaml.translate "buffer" ocaml_ast in
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
      Some (LiquidInit.compile_liquid_init env syntax_ast syntax_init)
  in

  ( env, syntax_ast, pre_michelson, pre_init )


let run_pre env pre_michelson input storage =
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
  ] @ (match !LiquidOptions.source with
      | None -> []
      | Some source -> ["contract", Printf.sprintf "%S" source]
    )
  in
  let run_json =
    run_fields
    |> List.map (fun (f,v) -> "\"" ^ f ^ "\":" ^ v)
    |> String.concat ","
    |> fun fs -> "{" ^ fs ^ "}"
  in
  let r =
    request ~data:run_json "/blocks/prevalidation/proto/helpers/run_code"
    |> Ezjsonm.from_string
  in
  try
    let err = Ezjsonm.find r ["error"] in
    let err_s = Data_encoding_ezjsonm.to_string err in
    raise (RequestError err_s)
  with Not_found ->
  try
    let storage_r = Ezjsonm.find r ["ok"; "storage"] in
    let result_r = Ezjsonm.find r ["ok"; "output"] in
    let storage_expr = LiquidToTezos.const_of_ezjson storage_r in
    let result_expr = LiquidToTezos.const_of_ezjson result_r in
    let env = LiquidTezosTypes.empty_env env.filename in
    let storage =
      LiquidFromTezos.convert_const_type env storage_expr pre_michelson.storage
    in
    let result =
      LiquidFromTezos.convert_const_type env result_expr pre_michelson.return
    in
    (result, storage)
  with Not_found ->
    raise (RequestError "Bad response for run")


let run liquid input_string storage_string =
  let env, syntax_ast, pre_michelson, pre_init = compile_liquid liquid in
  let input =
    LiquidData.translate env syntax_ast input_string pre_michelson.parameter
  in
  let storage =
    LiquidData.translate env syntax_ast storage_string pre_michelson.storage
  in
  run_pre env pre_michelson input storage
