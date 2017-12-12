(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val translate_const_exp : location -> encoded_exp -> const

val data_of_liq :
  filename:string ->
  contract:string ->
  parameter:string ->
  storage:string -> (string,error) result * (string,error) result

val translate :
  env -> syntax_contract -> string -> datatype -> const
