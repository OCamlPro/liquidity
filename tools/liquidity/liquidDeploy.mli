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

type big_map_diff_item =
  | Big_map_add of LiquidTypes.const * LiquidTypes.const
  | Big_map_remove of LiquidTypes.const

type big_map_diff = big_map_diff_item list

type stack_item =
  | StackConst of LiquidTypes.const
  | StackCode of int

type trace_item = {
  loc : LiquidTypes.location option;
  gas : int;
  stack : stack_item list;
}

type trace = trace_item array

val request : (?data:string -> string -> string Lwt.t) ref

module type S = sig
  type 'a t

  (** Run contract with given parameter and storage on the Tezos node specified
     in ![LiquidOptions], returns the return value, the storage and a diff of a
     big map id the contract contains any *)
  val run :
    from -> string -> string ->
    (LiquidTypes.const * big_map_diff option) t

  val run_debug :
    from -> string -> string ->
    (LiquidTypes.const * big_map_diff option * trace) t

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

  val activate : secret:string -> string t

end

module Async : S with type 'a t = 'a Lwt.t

module Sync : S with type 'a t = 'a
