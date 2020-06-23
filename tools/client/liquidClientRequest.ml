
exception RequestError of int * string
exception ResponseError of string

open Lwt

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

(*      (Ezjsonm.to_string ~minify:false (Ezjsonm.from_string data)); *)

let curl_call meth f data path =
  let host = !LiquidOptions.node in
  if !LiquidOptions.verbosity > 0 then
    Printf.eprintf "\n%s to %s%s:\n--------------\n<<<%s>>>\n%!"
      meth host path data;
  try
    f host path data >>= fun (status, json) ->
    if !LiquidOptions.verbosity > 0 then begin
      Printf.eprintf "\nNode Response %d:\n------------------\n<<<%s>>>\n%!"
        status json;
    end;
    if status <> 200 then raise (RequestError (status, json));
    return json
  with Curl.CurlException (code, i, s) (* as exn *) ->
    raise (RequestError (Curl.errno code, s))

let curl_post ~data path =
  curl_call "POST" Network.post data path

let curl_get path =
  curl_call "GET" (fun host path data -> Network.get host path) "" path


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
  lazy (
    Lwt.catch
      (fun () -> !get "/errors" >|= Ezjsonm.from_string)
      (function
        | RequestError _ | Not_found -> return @@ `O []
        | exn -> Lwt.fail exn)
  )
