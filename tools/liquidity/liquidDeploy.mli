(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type from =
  | From_strings of string list
  | From_files of string list

type key_diff =
  | DiffKeyHash of string
  | DiffKey of LiquidTypes.const

type big_map_diff_item =
  | Big_map_add of key_diff * LiquidTypes.const
  | Big_map_remove of key_diff

type big_map_diff = big_map_diff_item list

type stack_item =
  | StackConst of LiquidTypes.const
  | StackCode of int

type trace_item = {
  loc : LiquidTypes.location option;
  gas : int;
  stack : (stack_item * string option) list;
}

type trace = trace_item array

type internal_operation =
  | Reveal of string
  | Transaction of {
      amount : string;
      destination : string;
      parameters : LiquidTypes.const option;
    }
  | Origination of {
      manager: string ;
      delegate: string option ;
      script: (LiquidTypes.typed_contract * LiquidTypes.const) option ;
      spendable: bool ;
      delegatable: bool ;
      balance: string ;
    }
  | Delegation of string option

type operation = {
  source : string;
  nonce : int;
  op : internal_operation;
}

exception RequestError of int * string
exception ResponseError of string
exception RuntimeError of LiquidTypes.error * trace option
exception LocalizedError of LiquidTypes.error
exception RuntimeFailure of LiquidTypes.error * string option * trace option

val post : (data:string -> string -> string Lwt.t) ref
val get : (string -> string Lwt.t) ref

module type S = sig
  type 'a t

  (** Run contract with given parameter and storage on the Tezos node specified
     in ![LiquidOptions], returns the return value, the storage and a diff of a
     big map id the contract contains any *)
  val run :
    from -> string -> string -> string ->
    (operation list * LiquidTypes.const * big_map_diff option) t

  val run_debug :
    from -> string -> string -> string ->
    (operation list * LiquidTypes.const * big_map_diff option * trace) t

  (** Compute the initial storage for a specific script, returns storage data *)
  val init_storage : from -> string list -> LiquidTypes.const t

  (** Forge a deployment operation contract on the Tezos node specified in
      ![LiquidOptions], returns the hex-encoded operation *)
  val forge_deploy :
    ?delegatable:bool -> ?spendable:bool -> from -> string list -> string t

  (** Deploy a Liquidity contract on the Tezos node specified in
      ![LiquidOptions], returns the operation hash and the contract address *)
  val deploy :
    ?delegatable:bool -> ?spendable:bool -> from -> string list ->
    (string * (string, exn) result) t

  val get_storage : from -> string -> LiquidTypes.const t

  (** Forge an operation to call a deploy contract, returns the hex-encoded
      operation *)
  val forge_call : from -> string -> string -> string -> string t

  (** Calls a deployed Liquidity contract on the Tezos node specified in
      ![LiquidOptions], returns the operation hash *)
  val call : from -> string -> string -> string ->
    (string * (unit, exn) result) t

  val activate : secret:string -> string t

end

module Async : S with type 'a t = 'a Lwt.t

module Sync : S with type 'a t = 'a
