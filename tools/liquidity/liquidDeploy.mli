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
exception LocalizedError of LiquidTypes.error
exception RuntimeFailure of LiquidTypes.error * string option

type from =
  | From_string of string
  | From_file of string

val request : (?data:string -> string -> string Lwt.t) ref

module type S = sig
  type 'a t

  (** Run contract with given parameter and storage on the Tezos node specified
      in ![LiquidOptions], returns a pair containig the return value and the
      storage *)
  val run :
    from -> string -> string -> (LiquidTypes.const * LiquidTypes.const) t

  (** Forge a deployment operation contract on the Tezos node specified in
      ![LiquidOptions], returns the hex-encoded operation *)
  val forge_deploy :
    ?delegatable:bool -> ?spendable:bool -> from -> string list -> string t

  (** Deploy a Liquidity contract on the Tezos node specified in
      ![LiquidOptions], returns the operation hash and the contract address *)
  val deploy :
    ?delegatable:bool -> ?spendable:bool -> from -> string list ->
    (string * string) t

  val get_storage : from -> string -> LiquidTypes.const t

  (** Forge an operation to call a deploy contract, returns the hex-encoded
      operation *)
  val forge_call : from -> string -> string -> string t

  (** Calls a deployed Liquidity contract on the Tezos node specified in
      ![LiquidOptions], returns the operation hash *)
  val call : from -> string -> string -> string t

  val faucet_to : string -> unit t

end

module Async : S with type 'a t = 'a Lwt.t

module Sync : S with type 'a t = 'a
