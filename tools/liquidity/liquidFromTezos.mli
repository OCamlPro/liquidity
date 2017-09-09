(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val convert_contract :
  Script_repr.code -> pre_michelson contract

val contract_of_string : string -> Tezos_context.Script.code option

val data_of_string : string -> Tezos_context.Script.expr option
val pp : Format.formatter -> Tezos_context.error -> unit
