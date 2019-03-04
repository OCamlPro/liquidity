(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val encode_type : ?decompiling:bool -> datatype -> datatype

val encode_contract :
  ?annot:bool -> ?decompiling:bool -> typed_contract ->
  encoded_contract * encoded_exp StringMap.t

val encode_code : typecheck_env -> typed_exp -> encoded_exp

val encode_const : env -> full_contract_sig -> typed_const -> encoded_const

val encode_contract_sig : contract_sig -> datatype
