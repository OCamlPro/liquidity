(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val default_const : datatype -> const

val translate_const_exp : encoded_exp -> const

val translate :
  env -> contract_sig -> datatype -> string -> datatype -> const

val string_of_const : ?ty:datatype -> const -> string
