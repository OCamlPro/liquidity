(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val convert_contract : LiquidTypes.noloc_michelson_contract ->
                       LiquidTezosTypes.contract

val line_of_contract : LiquidTezosTypes.contract -> string
val string_of_contract : LiquidTezosTypes.contract -> string

val read_tezos_file :
  string ->
  LiquidTezosTypes.contract * LiquidTezosTypes.hash * LiquidTezosTypes.env

val arg_list : bool ref ->
         (Arg.key * Arg.spec * Arg.doc) list
