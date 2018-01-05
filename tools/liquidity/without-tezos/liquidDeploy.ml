(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception RequestError of string

type from =
  | From_string of string
  | From_file of string

let request = ref (fun ?data _ ->
  failwith "mini version cannot request")

module type S = sig
  type 'a t
  val run : from -> string -> string -> (LiquidTypes.const * LiquidTypes.const) t
  val forge_deploy : from -> string list -> string t
  val deploy : from -> string list -> (string * string) t
  val get_storage : from -> string -> LiquidTypes.const t
  val forge_call : from -> string -> string -> string t
  val call : from -> string -> string -> string t
end

module Dummy = struct

  let run _ _ _ =
    failwith "mini version cannot run"

  let forge_deploy _ _ =
    failwith "mini version cannot deploy"

  let deploy _ _ =
    failwith "mini version cannot deploy"

  let get_storage _ _ =
    failwith "mini version cannot query node"

  let forge_call _ _ _ =
    failwith "mini version cannot call"

  let call _ _ _ =
    failwith "mini version cannot call"
end

module Async = struct include Dummy type 'a t = 'a Lwt.t end

module Sync = struct include Dummy type 'a t = 'a end
