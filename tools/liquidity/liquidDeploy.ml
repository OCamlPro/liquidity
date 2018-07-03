(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open Lwt
open Michelson_Tezos (* for crypto *)

exception RequestError of int * string
exception ResponseError of string
exception RuntimeError of error
exception LocalizedError of error
exception RuntimeFailure of error * string option

type from =
  | From_string of string
  | From_file of string

type big_map_diff_item =
  | Big_map_add of const * const
  | Big_map_remove of const

type big_map_diff = big_map_diff_item list

type stack_item =
  | StackConst of const
  | StackCode of int

type trace_item = {
  loc : location option;
  gas : int;
  stack : (stack_item * string option) list;
}

type trace = trace_item array

module type S = sig
  type 'a t
  val run : from -> string -> string ->
    (int * LiquidTypes.const * big_map_diff option) t
  val run_debug : from -> string -> string ->
    (int * LiquidTypes.const * big_map_diff option * trace) t
  val forge_deploy : ?delegatable:bool -> ?spendable:bool ->
    from -> string list -> string t
  val deploy : ?delegatable:bool -> ?spendable:bool ->
    from -> string list -> (string * string) t
  val get_storage : from -> string -> LiquidTypes.const t
  val forge_call : from -> string -> string -> string t
  val call : from -> string -> string -> string t
  val activate : secret:string -> string t
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

  let get ?(content_type = "application/json") host path =
    let r, c = initialize_connection host path in
    Curl.set_post c false;
    Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
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
    (* if cc <> Curl.CURLE_OK then
     *   raise (RequestError
     *            (Printf.sprintf "[%d] [%s] Curl exception: %s\n%!"
     *                (Curl.errno cc) host path))
     * else *)
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    Lwt.return (rc, (Buffer.contents r))

  let get ?(content_type = "application/json") host path =
    let r, c = initialize_connection host path in
    Curl.set_post c false;
    Curl.set_httpheader c [ "Content-Type: " ^ content_type ];
    Curl_lwt.perform c >>= fun cc ->
    (* if cc <> Curl.CURLE_OK then
     *   raise (RequestError
     *            (Printf.sprintf "[%d] [%s] Curl exception: %s\n%!"
     *                (Curl.errno cc) host path))
     * else *)
    let rc = Curl.get_responsecode c in
    Curl.cleanup c;
    Lwt.return (rc, (Buffer.contents r))
end

let curl_post ~data path =
  let host = !LiquidOptions.tezos_node in
  if !LiquidOptions.verbosity > 0 then
    Printf.eprintf "\nPOST to %s%s:\n--------------\n%s\n%!"
      host path
      (* data; *)
      (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string data));
  try
    Network.post host path data >>= fun (status, json) ->
    if status <> 200 then raise (RequestError (status, json));
    if !LiquidOptions.verbosity > 0 then
      Printf.eprintf "\nNode Response %d:\n------------------\n%s\n%!"
        status
        (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string json));
    return json
  with Curl.CurlException (code, i, s) (* as exn *) ->
     raise (RequestError (Curl.errno code, s))

let curl_get path =
  let host = !LiquidOptions.tezos_node in
  if !LiquidOptions.verbosity > 0 then
    Printf.eprintf "\nGET to %s%s:\n--------------\\n%!"
      host path;
  try
    Network.get host path >>= fun (status, json) ->
    if status <> 200 then raise (RequestError (status, json));
    if !LiquidOptions.verbosity > 0 then
      Printf.eprintf "\nNode Response %d:\n------------------\n%s\n%!"
        status
        (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string json));
    return json
  with Curl.CurlException (code, i, s) (* as exn *) ->
     raise (RequestError (Curl.errno code, s))


let post = ref curl_post
let get = ref curl_get


(* let error_string_of_michelson_error json =
 *   let errors =  Ezjsonm.get_list Error_monad.error_of_json json in
 *   let fmt = Format.str_formatter in
 *   Michelson_v1_error_reporter.report_error
 *     ~details:false
 *     ~show_source:false
 *     fmt
 *     errors;
 *   Format.flush_str_formatter () *)

let error_schema =
  lazy (!get "/errors" >|= Ezjsonm.from_string)


let raise_error_from_l ?loc_table err_msg l =
  let default_error () =
    let last_descr = match List.rev l with
      | (_, _, _, _, Some descr) :: _ -> "\n  " ^ descr
      | _ -> ""
    in
    let err_l =
      List.map (fun (kind, id, _, title, descr) ->
          match title with
          | Some t -> t
          | None -> Printf.sprintf "%s: %s" kind id
        ) l
      |> String.concat "\n- "
    in
    Printf.sprintf "in %s\n- %s%s" err_msg err_l last_descr
  in
  match loc_table with
  | None -> raise (ResponseError (default_error ()))
  | Some loc_table ->
    let err_msg = Printf.sprintf "in %s" err_msg in
    try
      List.iter (fun (kind, id, loc, title, descr) ->
          match loc, kind, id with
          | Some loc, "temporary", "scriptRejectedRuntimeError" ->
            let err_loc, fail_str = List.assoc loc loc_table in
            raise (RuntimeFailure ({err_msg; err_loc}, fail_str))
          | Some loc, "temporary", _ ->
            let err_loc, _ = List.assoc loc loc_table in
            let title = match title with Some t -> t | None -> id in
            let err_msg = String.concat "\n- " [err_msg; title] in
            raise (RuntimeError {err_msg; err_loc})
          | Some loc, _, _ ->
            let err_loc, _ = List.assoc loc loc_table in
            let err_msg = default_error () in
            raise (LocalizedError {err_msg; err_loc})
          | _ -> ()
        ) l;
      raise (ResponseError (default_error ()))
    with Not_found -> raise (ResponseError (default_error ()))

let extract_errors_from_json r schema =
  let schema_l = Ezjsonm.find schema ["oneOf"] in
  try
    Ezjsonm.find r ["error"], schema_l
  with Not_found ->
  match Ezjsonm.get_list (fun x -> x) r with
  | err :: _ ->
    begin try
        let r = Ezjsonm.find err ["ecoproto"] in
        let id = Ezjsonm.find err ["id"] |> Ezjsonm.get_string in
        let schema_l =
          schema_l
          |> Ezjsonm.get_list (fun s ->
              try
                let s_id =
                  Ezjsonm.find s ["properties"; "id"; "enum"]
                  |> Ezjsonm.get_list Ezjsonm.get_string
                  |> function [s] -> s | _ -> assert false
                in
                if s_id <> id then
                  None
                else
                  Some (Ezjsonm.find s
                          ["properties"; "ecoproto"; "items"; "oneOf"])
              with Not_found -> None
            )
          |> List.find (function None -> false | Some _ -> true)
          |> function None -> assert false | Some s -> s
        in
        r, schema_l
      with Not_found  -> r, schema_l
    end
  | [] -> raise (ResponseError ("Could not parse error"))
  | exception Ezjsonm.Parse_error _ -> r, schema_l


let rec descr_of_id id schema =
  try
    schema
    |> Ezjsonm.get_list (fun s ->
        try
          let schema = Ezjsonm.find s ["oneOf"] in
          descr_of_id id schema
        with Not_found ->
        try
          let s_id =
            Ezjsonm.find s ["properties"; "id"; "enum"]
            |> Ezjsonm.get_list Ezjsonm.get_string
            |> function [s] -> s | _ -> assert false
          in
          if s_id <> id then
            None, None
          else (
            let t =
              try Some (Ezjsonm.find s ["title"] |> Ezjsonm.get_string)
              with Not_found -> None
            in
            let d =
              try Some (Ezjsonm.find s ["description"] |> Ezjsonm.get_string)
              with Not_found -> None
            in
            (t, d)
          )
        with Not_found ->
          None, None
      )
    |> List.find (function Some _, _ | _, Some _ -> true | _ -> false)
  with Not_found ->
    None, None

let raise_response_error ?loc_table msg r =
  Lazy.force error_schema >>= fun error_schema ->
  let err, schema = extract_errors_from_json r error_schema in
  let l =
    try
      Ezjsonm.get_list (fun err ->
          let kind = Ezjsonm.find err ["kind"] |> Ezjsonm.get_string in
          let id = Ezjsonm.find err ["id"] |> Ezjsonm.get_string in
          let title, descr = descr_of_id id schema in
          let loc =
            try Some (Ezjsonm.find err ["location"] |> Ezjsonm.get_int)
            with Not_found ->
            try Some (Ezjsonm.find err ["loc"] |> Ezjsonm.get_int)
            with Not_found -> None
          in
          kind, id, loc, title, descr
        ) err
    with Ezjsonm.Parse_error _ -> []
  in
  raise_error_from_l ?loc_table msg l


let send_post ?loc_table ~data path =
  Lwt.catch
    (fun () -> !post ~data path)
    (function
      | RequestError (code, res) as exn ->
        begin try raise_response_error ?loc_table path (Ezjsonm.from_string res)
          with Ezjsonm.Parse_error _ | Not_found -> Lwt.fail exn
        end
      | exn -> Lwt.fail exn
    )

let send_get ?loc_table path =
  Lwt.catch
    (fun () -> !get path)
    (function
      | RequestError (code, res) as exn ->
        begin try raise_response_error ?loc_table path (Ezjsonm.from_string res)
          with Ezjsonm.Parse_error _ | Not_found -> Lwt.fail exn
        end
      | exn -> Lwt.fail exn
    )


let mk_json_obj fields =
  fields
  |> List.map (fun (f,v) -> "\"" ^ f ^ "\":" ^ v)
  |> String.concat ","
  |> fun fs -> "{" ^ fs ^ "}"

let mk_json_arr l = "[" ^ String.concat "," l ^ "]"


let compile_liquid liquid =
  let ocaml_ast, filename = match liquid with
    | From_string s ->
      LiquidFromOCaml.structure_of_string ~filename:"liquidity_buffer" s,
      "liquidity_buffer"
    | From_file f -> LiquidFromOCaml.read_file f, f
  in
  let syntax_ast, syntax_init, env =
    LiquidFromOCaml.translate ~filename ocaml_ast in
  let contract_sig = syntax_ast.contract_sig in
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
        LiquidInit.compile_liquid_init env contract_sig syntax_init,
        inputs_infos)
  in

  ( env, syntax_ast, pre_michelson, pre_init )


let get_big_map_type contract_sig =
  match contract_sig.storage with
  | Ttuple (Tbigmap (k, v) :: _)
  | Trecord (_, (_, Tbigmap (k, v)) :: _) -> Some (k, v)
  | _ -> None

let memo_stack_code_cpt = ref 0
let memo_stack_code_tbl = Hashtbl.create 19
let reset_memo_stack_code () =
  memo_stack_code_cpt := 0;
  Hashtbl.clear memo_stack_code_tbl
let memo_stack_code e =
  try Hashtbl.find memo_stack_code_tbl e
  with Not_found ->
    let cpt = !memo_stack_code_cpt in
    incr memo_stack_code_cpt;
    Hashtbl.add memo_stack_code_tbl e cpt;
    cpt

let name_of_var_annot = function
  | None -> None
  | Some annot ->
    try Scanf.sscanf annot "@%s" (function
        | "" -> None
        | s -> Some s
      )
    with Scanf.Scan_failure _ | End_of_file -> None

let convert_stack env stack_expr =
  List.map (fun (e, annot) ->
      let name = name_of_var_annot annot in
      try StackConst (LiquidFromTezos.convert_const_notype env e), name
      with _ -> StackCode (memo_stack_code e), name
    ) stack_expr

let run_pre ?(debug=false)
    env contract_sig pre_michelson source input storage =
  let rpc = if debug then "trace_code" else "run_code" in
  let c, loc_table =
    LiquidToTezos.convert_contract ~expand:true pre_michelson in
  let input_m = LiquidToTezos.convert_const input in
  let storage_m = LiquidToTezos.convert_const storage in
  let contract_json = LiquidToTezos.json_of_contract c in
  let input_json = LiquidToTezos.json_of_const input_m in
  let storage_json = LiquidToTezos.json_of_const storage_m in
  let run_fields = [
    "script", contract_json;
    "input", input_json;
    "storage", storage_json;
    "amount", Printf.sprintf "%S" !LiquidOptions.amount;
  ] in
  let run_json = mk_json_obj run_fields in
  send_post ~loc_table ~data:run_json
    (Printf.sprintf "/chains/main/blocks/head/helpers/scripts/%s" rpc)
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let storage_r = Ezjsonm.find r ["storage"] in
    let operations_r = Ezjsonm.find r ["operations"] in
    let nb_operations = Ezjsonm.get_list (fun x -> x) operations_r
                        |> List.length in
    let big_map_diff_r =
      try Some (Ezjsonm.find r ["big_map_diff"])
      with Not_found -> None
    in
    let trace_r =
      if not debug then None
      else Some (Ezjsonm.find r ["trace"])
    in
    let storage_expr = LiquidToTezos.const_of_ezjson storage_r in
    let big_map_diff_expr = match big_map_diff_r with
      | None -> None
      | Some json_diff ->
        Some (Ezjsonm.get_list (fun pair ->
            Ezjsonm.get_pair
              (fun k -> LiquidToTezos.const_of_ezjson k)
              (fun v ->
                 match Ezjsonm.get_dict v with
                 | [] -> None
                 | _ :: _ ->
                   Some (LiquidToTezos.const_of_ezjson v)
                 | exception Ezjsonm.Parse_error _ ->
                   Some (LiquidToTezos.const_of_ezjson v))
              pair
          ) json_diff)
    in
    let trace_expr = match trace_r with
      | None -> None
      | Some trace_r ->
        Some (Ezjsonm.get_list (fun step ->
            let loc = Ezjsonm.find step ["location"] |> Ezjsonm.get_int in
            let gas = Ezjsonm.find step ["gas"]
                      |> Ezjsonm.get_string |> int_of_string in
            let stack =
              Ezjsonm.find step ["stack"]
              |> Ezjsonm.get_list (fun s ->
                  Ezjsonm.find s ["item"] |> LiquidToTezos.const_of_ezjson,
                  try Some (Ezjsonm.find s ["annot"] |> Ezjsonm.get_string)
                  with Not_found -> None
                )
            in
            (loc, gas, stack)
          ) trace_r)
    in
    let env = LiquidTezosTypes.empty_env env.filename in
    let storage =
      LiquidFromTezos.convert_const_type env storage_expr
        contract_sig.storage
    in
    (* TODO parse returned operations *)
    let big_map_diff =
      match big_map_diff_expr, get_big_map_type contract_sig with
      | None, _ -> None
      | Some _, None -> assert false
      | Some d, Some (tk, tv) ->
        Some (List.map (function
            | k, Some v ->
              Big_map_add (LiquidFromTezos.convert_const_type env k tk,
                           LiquidFromTezos.convert_const_type env v tv)
            | k, None ->
              Big_map_remove (LiquidFromTezos.convert_const_type env k tk)
          ) d)
    in
    let trace = match trace_expr with
      | None -> None
      | Some trace_expr ->
        let l =
          List.map (fun (loc, gas, stack) ->
              let loc =  match List.assoc_opt loc loc_table with
                | Some (loc, _) -> Some loc
                | None -> None
              in
              (* we don't know the liquidity type of elements in the stack *)
              let stack = convert_stack env stack in
              { loc; gas; stack }
            ) trace_expr in
        reset_memo_stack_code ();
        Some (Array.of_list l)
    in
    return (nb_operations, storage, big_map_diff, trace)
  with Not_found ->
    raise_response_error ~loc_table "run" r


let run ~debug liquid input_string storage_string =
  let env, { contract_sig }, pre_michelson, _ = compile_liquid liquid in
  let input =
    LiquidData.translate { env with filename = "run_input" }
      contract_sig input_string contract_sig.parameter
  in
  let storage =
    LiquidData.translate { env with filename = "run_storage" }
      contract_sig storage_string contract_sig.storage
  in
  run_pre ~debug env contract_sig
    pre_michelson !LiquidOptions.source input storage

let run_debug liquid input_string storage_string =
  run ~debug:true liquid input_string storage_string
  >>= function
  | (nbops, sto, big_diff, Some trace) ->
    Lwt.return (nbops, sto, big_diff, trace)
  | _ -> assert false

let run liquid input_string storage_string =
  run ~debug:false liquid input_string storage_string
  >>= fun (nbops, sto, big_diff, _) ->
  Lwt.return (nbops, sto, big_diff)

let get_json_string s =
  try Scanf.sscanf s "%S" (fun x -> x)
  with _ -> raise Not_found

let get_json_int s =
  try Scanf.sscanf s "%d" (fun x -> x)
  with _ ->
  try Scanf.sscanf s "\"%d\"" (fun x -> x)
  with _ -> raise Not_found

let get_counter source =
  send_get
      (Printf.sprintf "/chains/main/blocks/head/context/contracts/%s/counter"
         source)
  >>= fun r ->
  try
    get_json_int r |> return
  with Not_found ->
    raise_response_error "get_counter" (Ezjsonm.from_string r)


let get_head_hash () =
  send_get "/chains/main/blocks/head/header" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["hash"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_response_error "get_head_hash" r

type head = {
  head_hash : string;
  head_chain_id : string;
}

let get_head () =
  send_get "/chains/main/blocks/head" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let head_hash = Ezjsonm.find r ["hash"] |> Ezjsonm.get_string in
    let head_chain_id = Ezjsonm.find r ["chain_id"] |> Ezjsonm.get_string in
    return { head_hash; head_chain_id }
  with Not_found ->
    raise_response_error "get_head" r

let get_predecessor () =
  send_get "/chains/main/blocks/head/header" >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    Ezjsonm.find r ["predecessor"] |> Ezjsonm.get_string |> return
  with Not_found ->
    raise_response_error "get_predecessor" r



let get_storage liquid address =
  let env, syntax_ast, pre_michelson, pre_init_infos = compile_liquid liquid in
  send_get
    (Printf.sprintf
       "/chains/main/blocks/head/context/contracts/%s/storage"
       address)
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    let storage_expr = LiquidToTezos.const_of_ezjson r in
    let env = LiquidTezosTypes.empty_env env.filename in
    return
      (LiquidFromTezos.convert_const_type env storage_expr
         syntax_ast.contract_sig.storage)
  with Not_found ->
    raise_response_error "get_storage" r

let is_revealed source =
  send_get
    (Printf.sprintf
       "/chains/main/blocks/head/context/contracts/%s/manager_key"
       source)
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  try
    ignore (Ezjsonm.find r ["key"]);
    return true
  with Not_found ->
    return false

let get_public_key_hash_from_secret_key sk =
  sk
  |> Sodium.Sign.secret_key_to_public_key
  (* Replace by this when tezos is fixed *)
  (* |> Ed25519.Secret_key.to_public_key *)
  |> Ed25519.Public_key.hash
  |> Ed25519.Public_key_hash.to_b58check

let get_public_key_from_secret_key sk =
  sk
  |> Sodium.Sign.secret_key_to_public_key
  (* Replace by this when tezos is fixed *)
  (* |> Ed25519.Secret_key.to_public_key *)
  |> Ed25519.Public_key.to_b58check

let forge_deploy ?head ?source ?public_key
    ?(delegatable=false) ?(spendable=false)
    liquid init_params_strings =
  let source = match source, !LiquidOptions.source with
    | Some source, _ | _, Some source -> source
    | None, None -> raise (ResponseError "forge_deploy: Missing source")
  in
  let env, syntax_ast, pre_michelson, pre_init_infos = compile_liquid liquid in
  let contract_sig = syntax_ast.contract_sig in
  let pre_init, init_infos = match pre_init_infos with
    | None -> raise (ResponseError "forge_deploy: Missing init")
    | Some pre_init_infos -> pre_init_infos
  in
  let init_storage_lwt = match pre_init with
    | LiquidInit.Init_constant c ->
      if init_params_strings <> [] then
        raise (ResponseError "forge_deploy: Constant storage, no inputs needed");
      return c
    | LiquidInit.Init_code (syntax_c, c) ->
      let init_params =
        try
          List.map2 (fun input_str (input_name,_, input_ty) ->
            LiquidData.translate { env with filename = input_name }
              contract_sig input_str input_ty
            ) init_params_strings init_infos
        with Invalid_argument _ ->
          raise
            (ResponseError
               (Printf.sprintf
                  "forge_deploy: init storage needs %d arguments, but was given %d"
                  (List.length init_infos) (List.length init_params_strings)
               ))
      in
      let eval_input_storage =
        try
          LiquidData.default_const contract_sig.storage
          |> LiquidEncode.encode_const env contract_sig
        with Not_found -> failwith "could not construct dummy storage for eval"
      in
      let eval_input_parameter = match init_params with
        | [] -> CUnit
        | [x] -> x
        | _ -> CTuple init_params in

      run_pre env contract_sig c (Some source)
        eval_input_parameter eval_input_storage
      >>= fun (_, eval_init_storage, big_map_diff, _) ->
      (* Add elements of big map *)
      let eval_init_storage = match eval_init_storage, big_map_diff with
        | CTuple (CBigMap m :: rtuple), Some l ->
          let m = List.fold_left (fun m -> function
              | Big_map_add (k, v) -> (k, v) :: m
              | Big_map_remove _ -> m
            ) m l
          in
          CTuple (CBigMap m :: rtuple)
        | CRecord ((bname, CBigMap m) :: rrecord), Some l ->
          let m = List.fold_left (fun m -> function
              | Big_map_add (k, v) -> (k, v) :: m
              | Big_map_remove _ -> m
            ) m l
          in
          CRecord ((bname, CBigMap m) :: rrecord)
        | _ -> eval_init_storage
      in
      Printf.eprintf "Evaluated initial storage: %s\n%!"
        (LiquidData.string_of_const eval_init_storage);
      return (LiquidEncode.encode_const env contract_sig eval_init_storage)
  in
  init_storage_lwt >>= fun init_storage ->

  begin match head with
    | Some head -> return head
    | None -> get_head_hash ()
  end >>= fun head ->
  get_counter source >>= fun counter ->
  is_revealed source >>= fun source_revealed ->
  let counter = counter + 1 in
  let c, loc_table =
    LiquidToTezos.convert_contract ~expand:true pre_michelson in
  let init_storage_m = LiquidToTezos.convert_const init_storage in
  let contract_json = LiquidToTezos.json_of_contract c in
  let init_storage_json = LiquidToTezos.json_of_const init_storage_m in
  let script_json = [
    "code", contract_json;
    "storage", init_storage_json
  ] |> mk_json_obj
  in
  let origination_json counter = [
    "kind", "\"origination\"";
    "source", Printf.sprintf "%S" source;
    "fee", Printf.sprintf "%S" !LiquidOptions.fee;
    "counter", Printf.sprintf "\"%d\"" counter;
    "gas_limit", Printf.sprintf "%S" !LiquidOptions.gas_limit;
    "storage_limit", Printf.sprintf "%S" !LiquidOptions.storage_limit;
    "managerPubkey", Printf.sprintf "%S" source;
    "balance", Printf.sprintf "%S" !LiquidOptions.amount;
    "spendable", string_of_bool spendable;
    "delegatable", string_of_bool delegatable;
    "script", script_json;
  ] |> mk_json_obj
  in
  let operations = match source_revealed, public_key with
    | true, _ | _, None -> [origination_json counter]
    | false, Some edpk ->
      let reveal_json = [
        "kind", "\"reveal\"";
        "source", Printf.sprintf "%S" source;
        "fee", "\"0\"";
        "counter", Printf.sprintf "\"%d\"" counter;
        "gas_limit", "\"20\"";
        "storage_limit", "\"0\"";
        "public_key", Printf.sprintf "%S" edpk;
      ] |> mk_json_obj
      in
      [reveal_json; origination_json (counter + 1)]
  in
  let operations_json = mk_json_arr operations in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "contents", operations_json;
  ] |> mk_json_obj
  in
  send_post ~loc_table ~data
    "/chains/main/blocks/head/helpers/forge/operations"
  >>= fun r ->
  try
    let op = get_json_string r in
    return (op, operations_json, loc_table)
  with Not_found ->
    raise_response_error ~loc_table "forge_deploy" (Ezjsonm.from_string r)

let hash msg =
  Blake2B.(to_bytes (hash_bytes [MBytes.of_string "\x03"; msg]))

let sign sk op_b =
  Ed25519.sign sk (hash op_b)

let inject ?loc_table ?sk ~head ?(is_deploy=false) json_op op =
  let op_b = MBytes.of_string (Hex.to_string op) in
  let signed_op, op_hash, data = match sk with
    | None ->
      let op_hash =
        Operation_hash.to_b58check @@
        Operation_hash.hash_bytes [ op_b ] in
      op, op_hash, [[
        "protocol", "\"ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK\"";
        "branch", Printf.sprintf "%S" head;
        "contents", json_op;
      ] |> mk_json_obj] |> mk_json_arr

    | Some sk ->
      let signature_b = sign sk op_b in
      let signature = Ed25519.Signature.to_b58check signature_b in
      let signed_op_b = MBytes.concat "" [op_b; signature_b] in
      let signed_op = Hex.of_string (MBytes.to_string signed_op_b) in
      let op_hash =
        Operation_hash.to_b58check @@
        Operation_hash.hash_bytes [ signed_op_b ] in
      signed_op, op_hash, [[
        "protocol", "\"ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK\"";
        "branch", Printf.sprintf "%S" head;
        "contents", json_op;
        "signature", Printf.sprintf "%S" signature;
      ] |> mk_json_obj] |> mk_json_arr
  in
  send_post ?loc_table ~data
    "/chains/main/blocks/head/helpers/preapply/operations"
  >>= fun r ->
  let r = Ezjsonm.from_string r in
  (try
     let r =
       match Ezjsonm.get_list (fun x -> x) r with
       | r :: _ -> r | [] -> assert false in
     Ezjsonm.find r ["contents"] |> Ezjsonm.get_list (fun o ->
         if is_deploy &&
            Ezjsonm.find o ["kind"] |> Ezjsonm.get_string = "origination"
         then try
             Ezjsonm.find o
               ["metadata"; "operation_result"; "originated_contracts"]
             |> Ezjsonm.get_list Ezjsonm.get_string
           with Not_found -> []
         else []
       )
     |> List.flatten
     |> return
   with Not_found ->
     raise_response_error ?loc_table "inject (preapply/operations)" r
  ) >>= fun contracts ->
  let data = Printf.sprintf "%S" (Hex.show signed_op) in
  send_post ?loc_table ~data "/injection/operation" >>= fun r ->
  (try
     get_json_string r |> return
   with Not_found ->
     raise_response_error ?loc_table "inject (/injection/operation)"
       (Ezjsonm.from_string r)
  ) >>= fun injected_op_hash ->
  assert (injected_op_hash = op_hash);

  return (injected_op_hash, contracts)


let deploy ?(delegatable=false) ?(spendable=false) liquid init_params_strings =
  let sk = match !LiquidOptions.private_key with
    | None -> raise (ResponseError "deploy: Missing private key")
    | Some sk -> match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ -> raise (ResponseError "deploy: Bad private key")
  in
  let source = match !LiquidOptions.source with
    | Some source -> source
    | None -> get_public_key_hash_from_secret_key sk
  in
  let public_key = get_public_key_from_secret_key sk in
  get_head_hash () >>= fun head ->
  forge_deploy ~head ~source ~public_key ~delegatable ~spendable
    liquid init_params_strings
  >>= fun (op, op_json, loc_table) ->
  inject ~loc_table ~sk ~head ~is_deploy:true op_json (`Hex op) >>= function
  | op_h, [c] -> return (op_h, c)
  | _ -> raise (ResponseError "deploy (inject)")


let forge_call ?head ?source ?public_key liquid address parameter_string =
  let source = match source, !LiquidOptions.source with
    | Some source, _ | _, Some source -> source
    | None, None -> raise (ResponseError "forge_call: Missing source")
  in
  let env, { contract_sig }, pre_michelson, pre_init_infos =
    compile_liquid liquid in
  let parameter =
    LiquidData.translate { env with filename = "call_parameter" }
      contract_sig parameter_string contract_sig.parameter
  in
  let _, loc_table =
    LiquidToTezos.convert_contract ~expand:true pre_michelson in
  let parameter_m = LiquidToTezos.convert_const parameter in
  let parameter_json = LiquidToTezos.json_of_const parameter_m in
  begin match head with
    | Some head -> return head
    | None -> get_head_hash ()
  end >>= fun head ->
  get_counter source >>= fun counter ->
  is_revealed source >>= fun source_revealed ->
  let counter = counter + 1 in
  let transaction_json counter = [
    "kind", "\"transaction\"";
    "source", Printf.sprintf "%S" source;
    "fee", Printf.sprintf "%S" !LiquidOptions.fee;
    "counter", Printf.sprintf "\"%d\"" counter;
    "gas_limit", Printf.sprintf "%S" !LiquidOptions.gas_limit;
    "storage_limit", Printf.sprintf "%S" !LiquidOptions.storage_limit;
    "amount", Printf.sprintf "%S" !LiquidOptions.amount;
    "destination", Printf.sprintf "%S" address;
    "parameters", parameter_json;
  ] |> mk_json_obj
  in
  let operations = match source_revealed, public_key with
    | true, _ | _, None -> [transaction_json counter]
    | false, Some edpk ->
      let reveal_json = [
        "kind", "\"reveal\"";
        "source", Printf.sprintf "%S" source;
        "fee", "\"0\"";
        "counter", Printf.sprintf "\"%d\"" counter;
        "gas_limit", "\"20\"";
        "storage_limit", "\"0\"";
        "public_key", Printf.sprintf "%S" edpk;
      ] |> mk_json_obj
      in
      [reveal_json; transaction_json (counter + 1)]
  in
  let operations_json = mk_json_arr operations in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "contents", operations_json;
  ] |> mk_json_obj
  in
  send_post ~loc_table ~data
    "/chains/main/blocks/head/helpers/forge/operations"
  >>= fun r ->
  try
    let op = get_json_string r in
    return (op, operations_json, loc_table)
  with Not_found ->
    raise_response_error ~loc_table "forge_call" (Ezjsonm.from_string r)


let call liquid address parameter_string =
  let sk = match !LiquidOptions.private_key with
    | None -> raise (ResponseError "call: Missing private key")
    | Some sk -> match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ -> raise (ResponseError "call: Bad private key")
  in
  let source = match !LiquidOptions.source with
    | Some source -> source
    | None -> get_public_key_hash_from_secret_key sk
  in
  let public_key = get_public_key_from_secret_key sk in
  get_head_hash () >>= fun head ->
  forge_call ~head ~source ~public_key
    liquid address parameter_string
  >>= fun (op, op_json, loc_table) ->
  inject ~loc_table ~sk ~head op_json (`Hex op) >>= function
  | op_h, [] -> return op_h
  | _ -> raise (ResponseError "call (inject)")


let reveal sk =
  let source = get_public_key_hash_from_secret_key sk in
  let public_key = get_public_key_from_secret_key sk in
  get_head_hash () >>= fun head ->
  get_counter source >>= fun counter ->
  let counter = counter + 1 in
  let reveal_json = [
    "kind", "\"reveal\"";
    "source", Printf.sprintf "%S" source;
    "fee", "\"0\"";
    "counter", Printf.sprintf "\"%d\"" counter;
    "gas_limit", "\"20\"";
    "storage_limit", "\"0\"";
    "public_key", Printf.sprintf "%S" public_key;
  ] |> mk_json_obj
  in
  let operations_json = mk_json_arr [reveal_json] in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "contents", operations_json;
  ] |> mk_json_obj
  in
  send_post ~data "/chains/main/blocks/head/helpers/forge/operations"
  >>= fun r ->
  (try
     get_json_string r |> return_some
   with Not_found ->
     return_none
  ) >>= function
  | Some op ->
    inject ~sk ~head operations_json (`Hex op) >>= fun _ ->
    return_unit
  | None ->
    return_unit


let activate ~secret =
  let sk = match !LiquidOptions.private_key with
    | None -> raise (ResponseError "activate: Missing private key")
    | Some sk -> match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ -> raise (ResponseError "activate: Bad private key")
  in
  let source = match !LiquidOptions.source with
    | Some source -> source
    | None -> get_public_key_hash_from_secret_key sk
  in
  get_head_hash () >>= fun head ->
  let activate_json = [
    "kind", "\"activate_account\"";
    "pkh", Printf.sprintf "%S" source;
    "secret", Printf.sprintf "%S" secret;
  ] |> mk_json_obj
  in
  let operations_json = mk_json_arr [activate_json] in
  let data = [
    "branch", Printf.sprintf "%S" head;
    "contents", operations_json;
  ] |> mk_json_obj
  in
  send_post ~data "/chains/main/blocks/head/helpers/forge/operations"
  >>= fun r ->
  (try
     get_json_string r |> return
   with Not_found ->
     raise_response_error "forge activation" (Ezjsonm.from_string r)
  ) >>= fun op ->
  inject ~sk ~head operations_json (`Hex op) >>= function
  | _, _::_ -> raise (ResponseError "activation (inject)")
  | op_h, [] -> return op_h


(* Withoud optional argument head *)
module Async = struct
  type 'a t = 'a Lwt.t

  let forge_deploy ?(delegatable=false) ?(spendable=false)
      liquid init_params_strings =
    forge_deploy ~delegatable ~spendable liquid init_params_strings
    >>= fun (op, _, _) -> return op

  let forge_call liquid address parameter_string =
    forge_call liquid address parameter_string
    >>= fun (op, _, _) -> return op

  let run liquid input_string storage_string =
    run liquid input_string storage_string

  let run_debug liquid input_string storage_string =
    run_debug liquid input_string storage_string

  let deploy ?(delegatable=false) ?(spendable=false)
      liquid init_params_strings =
    deploy ~delegatable ~spendable liquid init_params_strings

  let get_storage liquid address =
    get_storage liquid address

  let call liquid address parameter_string =
    call liquid address parameter_string

  let activate ~secret =
    activate ~secret
end

module Sync = struct
  type 'a t = 'a

  let forge_deploy ?(delegatable=false) ?(spendable=false)
      liquid init_params_strings =
    Lwt_main.run (forge_deploy liquid init_params_strings
                  >>= fun (op, _, _) -> return op)

  let forge_call liquid address parameter_string =
    Lwt_main.run (forge_call liquid address parameter_string
                  >>= fun (op, _, _) -> return op)

  let run liquid input_string storage_string =
    Lwt_main.run (run liquid input_string storage_string)

  let run_debug liquid input_string storage_string =
    Lwt_main.run (run_debug liquid input_string storage_string)

  let deploy ?(delegatable=false) ?(spendable=false)
      liquid init_params_strings =
    Lwt_main.run (deploy ~delegatable ~spendable liquid init_params_strings)

  let get_storage liquid address =
    Lwt_main.run (get_storage liquid address)

  let call liquid address parameter_string =
    Lwt_main.run (call liquid address parameter_string)

  let activate ~secret =
    Lwt_main.run (activate ~secret)

end
