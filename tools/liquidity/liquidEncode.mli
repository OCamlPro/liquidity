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
  warnings:bool -> env -> typed_contract ->
  encoded_contract * encoded_exp StringMap.t

val encode_code :
  warnings:bool -> env -> syntax_contract -> typed_exp -> encoded_exp
