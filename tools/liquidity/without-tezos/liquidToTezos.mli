(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Convert Michelson contracts/constants to Micheline
    contracts/constants for pretty-printing or use by the client. *)

type loc_table = (int * (LiquidTypes.location * string option)) list

(** Convert a Michelson contract to Micheline one. Also returns a
    table mapping Micheling locations to Liquidity source
    locations. This table is used to produce localized error messages
    and to localize errors returned by the Tezos node. *)
val convert_contract :
  expand:bool -> LiquidTypes.loc_michelson_contract ->
  LiquidTezosTypes.contract * loc_table

(** Convert a Michelson constant to Micheline one *)
val convert_const :
  expand:bool -> LiquidTypes.loc_michelson LiquidTypes.const -> LiquidTezosTypes.expr

(** {2 Pretty printing Micheline } *)

(** {3 Pretty printing Micheline to string } *)

val line_of_contract : LiquidTezosTypes.contract -> string
val string_of_contract : LiquidTezosTypes.contract -> string
val json_of_contract : LiquidTezosTypes.contract -> string

(** {3 Pretty printing/parsing Micheline to/from Json } *)

val contract_of_json : string -> LiquidTezosTypes.contract
val contract_of_ezjson : LiquidTezosTypes.json -> LiquidTezosTypes.contract

val line_of_const : LiquidTezosTypes.expr -> string
val string_of_const : LiquidTezosTypes.expr -> string
val json_of_const : LiquidTezosTypes.expr -> string

val const_of_json : string -> LiquidTezosTypes.expr
val const_of_ezjson : LiquidTezosTypes.json -> LiquidTezosTypes.expr


val read_tezos_file : string -> LiquidTezosTypes.contract * LiquidTezosTypes.env
val read_tezos_json : string -> LiquidTezosTypes.contract * LiquidTezosTypes.env

val arg_list : bool ref ->
  (Arg.key * Arg.spec * Arg.doc) list
