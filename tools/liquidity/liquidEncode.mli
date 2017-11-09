(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val encode_type : ?keepalias:bool -> datatype -> datatype

val encode_contract :
  warnings:bool -> env -> typed_exp contract ->
  typed_exp contract * datatype exp StringMap.t

val encode_code :
  warnings:bool -> env -> syntax_exp contract -> typed_exp -> typed_exp
