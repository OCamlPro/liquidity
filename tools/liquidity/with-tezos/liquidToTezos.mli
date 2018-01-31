(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val convert_contract :
  expand:bool -> LiquidTypes.loc_michelson_contract ->
  LiquidTezosTypes.contract
val convert_const : LiquidTypes.const -> LiquidTezosTypes.expr

val line_of_contract : LiquidTezosTypes.contract -> string
val string_of_contract : LiquidTezosTypes.contract -> string
val json_of_contract : LiquidTezosTypes.contract -> string
val contract_of_json : string -> LiquidTezosTypes.contract
val contract_of_ezjson : LiquidTezosTypes.json -> LiquidTezosTypes.contract
val json_of_const : LiquidTezosTypes.expr -> string
val const_of_json : string -> LiquidTezosTypes.expr
val const_of_ezjson : LiquidTezosTypes.json -> LiquidTezosTypes.expr


val read_tezos_file : string -> LiquidTezosTypes.contract * LiquidTezosTypes.env

val arg_list : bool ref ->
         (Arg.key * Arg.spec * Arg.doc) list
