open LiquidClientSigs
open LiquidClientRequest
open LiquidClientUtils
open LiquidClientTypes
open LiquidClientErrors
open Lwt.Infix
open Dune_Network_Lib.Crypto

type rien = |

let default_raise_response_error ?loc_table:_ path r =
  let s =
    Printf.sprintf
      "in %s:\n%s"
      path
      (Ezjsonm.value_to_string ~minify:false r) in
  raise (ResponseError (s, r))

(* let send_post
 *     ~raise_response_error
 *     ?loc_table ~data path =
 *   Lwt.catch
 *     (fun () -> !post ~data path)
 *     (function
 *       | RequestError (code, res) as exn ->
 *         begin
 *           try raise_response_error ?loc_table path (Ezjsonm.from_string res)
 *           with Ezjsonm.Parse_error _ | Not_found -> Lwt.fail exn
 *         end
 *       | exn -> Lwt.fail exn
 *     )
 *
 * let send_get
 *     ~raise_response_error
 *     ?loc_table path =
 *   Lwt.catch
 *     (fun () -> !get path)
 *     (function
 *       | RequestError (code, res) as exn ->
 *         begin
 *           try raise_response_error ?loc_table path (Ezjsonm.from_string res)
 *           with Ezjsonm.Parse_error _ | Not_found -> Lwt.fail exn
 *         end
 *       | exn -> Lwt.fail exn
 *     ) *)

let post
    ?(raise_response_error=default_raise_response_error)
    ?loc_table ~input ~output ~path data =
  Format.kasprintf (fun path ->
      Lwt.catch
        (fun () ->
           let data =
             Json_encoding.construct input data
             |> Ezjsonm.value_to_string in
           !post ~data path >>= fun r ->
           let r = Ezjsonm.value_from_string r in
           try Json_encoding.destruct output r |> Lwt.return
           with Json_encoding.Cannot_destruct _ ->
             raise_response_error path r
        )
        (function
          | RequestError (code, res) as exn ->
            begin
              try raise_response_error ?loc_table path (Ezjsonm.from_string res)
              with Ezjsonm.Parse_error _ | Not_found -> raise exn
            end
          | exn -> raise exn
        )
    ) path

let get
    ?(raise_response_error=default_raise_response_error)
    ?loc_table ~output ~path =
  Format.kasprintf (fun path ->
      Lwt.catch
        (fun () ->
           !get path >>= fun r ->
           let r = Ezjsonm.value_from_string r in
           try Json_encoding.destruct output r |> Lwt.return
           with Json_encoding.Cannot_destruct _ ->
             raise_response_error path r
        )
        (function
          | RequestError (code, res) as exn ->
            begin
              try raise_response_error ?loc_table path (Ezjsonm.from_string res)
              with Ezjsonm.Parse_error _ | Not_found -> raise exn
            end
          | exn -> raise exn
        )
    ) path


let get_counter source =
  get
    ~output:int_string
    ~path:"/chains/main/blocks/head/context/contracts/%s/counter"
    (Signature.Public_key_hash.to_b58check source)

let get_head () =
  get
    ?raise_response_error:None
    ?loc_table:None
    ~output:Header.encoding
    ~path:"/chains/main/blocks/head/header"

let get_manager_key source =
  get
    ?raise_response_error:None
    ~output:Json_encoding.(option string)
    ~path:"/chains/main/blocks/head/context/contracts/%s/manager_key"
    (Signature.Public_key_hash.to_b58check source)

let get_balance addr =
  get
    ?raise_response_error:None
    ~output:tez_encoding
    ~path:"/chains/main/blocks/head/context/contracts/%s/balance"
    addr

let get_network () =
  get
    ?raise_response_error:None
    ?loc_table:None
    ~output:Json_encoding.(
        list
          (conv_ignore_extra (fun x -> x) (fun x -> x)
             (obj1 (req "chain_name" string)))
      )
    ~path:"/network/versions"

let get_constants () =
  get
    ?raise_response_error:None
    ?loc_table:None
    ~output:Constants.encoding
    ~path:"/chains/main/blocks/head/context/constants"

let injection ?loc_table bytes =
  post
    ?loc_table
    ~input:bytes_hex
    ~output:Json_encoding.string (* operation hash *)
    ~path:"/injection/operation"
    bytes

module Make(L : LANG) = struct

  module E = LiquidClientErrors.Make(L)
  module T = E.T

  open L
  open E
  open T

  let post ?loc_table ~input ~output ~path data =
    post ~raise_response_error ?loc_table ~input ~output ~path data

  let get ?loc_table ~output ~path =
    get ~raise_response_error ?loc_table ~output ~path

  let run ?loc_table data =
    post
      ?loc_table
      ~input:Run_code.Input.encoding
      ~output:Run_code.Result.encoding
      ~path:"/chains/main/blocks/head/helpers/scripts/run_code"
      data

  let trace ?loc_table data =
    post
      ?loc_table
      ~input:Run_code.Input.encoding
      ~output:Run_code.Result.encoding
      ~path:"/chains/main/blocks/head/helpers/scripts/trace_code"
      data

  let get_storage address =
    get
      ~output:Target.const_encoding
      ~path:"/chains/main/blocks/head/context/contracts/%s/storage"
      address

  let get_big_map_value id key =
    post
      ~input:Target.const_encoding
      key
      ~output:(Json_encoding.option Target.const_encoding)
      ~path:"/chains/main/blocks/head/context/big_maps/%d"
      id

  let get_big_map_hash_value id hash =
    Lwt.catch
      (fun () ->
         get
           ~output:(Json_encoding.option Target.const_encoding)
           ~path:"/chains/main/blocks/head/context/big_maps/%d/%s"
           id hash
      )
      (function
        | RequestError (404, _) -> Lwt.return_none
        | exn -> raise exn)

  let run_operation ?loc_table ~chain_id operation =
    post
      ?loc_table
      ~input:Run_operation.Input.encoding
      ~output:Run_operation.Result.encoding
      ~path:"/chains/main/blocks/head/helpers/scripts/run_operation"
      Run_operation.Input.{ operation; chain_id }

  let forge_operation ?loc_table data =
    post
      ?loc_table
      ~input:TargetOperation.encoding
      ~output:bytes_hex
      ~path:"/chains/main/blocks/head/helpers/forge/operations"
      data

  let preapply_operations ?loc_table ~protocol operations =
    post
      ?loc_table
      ~input:Json_encoding.(
          list @@ merge_objs
            (obj1 (req "protocol" string))
            TargetOperation.encoding)
      ~output:(Json_encoding.list Run_operation.Result.encoding)
      ~path:"/chains/main/blocks/head/helpers/preapply/operations"
      (List.map (fun op -> (protocol, op)) operations)

  let pack ~data ~ty =
    post
      ~input:Json_encoding.(
          obj2
            (req "data" Target.const_encoding)
            (req "type" Target.datatype_encoding)
        )
      ~output:Json_encoding.(conv_ignore_extra (fun x -> x) (fun x -> x)
                               (obj1 (req "packed" bytes_hex)))
      ~path:"/chains/main/blocks/head/helpers/scripts/pack_data"
      (data, ty)

end
