(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception RequestError of int * string
exception ResponseError of string
exception RuntimeError of LiquidTypes.error
exception RuntimeFailure of LiquidTypes.error * string option

type from =
  | From_string of string
  | From_file of string

let request = ref (fun ?data _ ->
  failwith "mini version cannot request")

module type S = sig
  type 'a t
  val run : from -> string -> string -> (LiquidTypes.const * LiquidTypes.const) t
  val forge_deploy : ?delegatable:bool -> ?spendable:bool ->
    from -> string list -> string t
  val deploy : ?delegatable:bool -> ?spendable:bool ->
    from -> string list -> (string * string) t
  val get_storage : from -> string -> LiquidTypes.const t
  val forge_call : from -> string -> string -> string t
  val call : from -> string -> string -> string t
  val faucet_to : string -> unit t
end

module Dummy = struct

  let run _ _ _ =
    failwith "mini version cannot run"

  let forge_deploy ?(delegatable=false) ?(spendable=false) _ _ =
    failwith "mini version cannot deploy"

  let deploy ?(delegatable=false) ?(spendable=false) _ _ =
    failwith "mini version cannot deploy"

  let get_storage _ _ =
    failwith "mini version cannot query node"

  let forge_call _ _ _ =
    failwith "mini version cannot call"

  let call _ _ _ =
    failwith "mini version cannot call"

  let faucet_to _ =
    failwith "mini version cannot faucet"
end

module Async = struct include Dummy type 'a t = 'a Lwt.t end

module Sync = struct include Dummy type 'a t = 'a end
