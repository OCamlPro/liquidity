(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val encode_type : datatype -> datatype

val encode_contract :
  ?annot:bool -> env -> typed_contract ->
  encoded_contract * encoded_exp StringMap.t

val encode_code : syntax_exp typecheck_env -> typed_exp -> encoded_exp

val encode_const : env -> syntax_contract -> const -> const
