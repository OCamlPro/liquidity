(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val default_const : datatype -> (datatype, 'a) exp const

(** same as [default_const] but with empty values for collections *)
val default_empty_const : datatype -> (datatype, 'a) exp const

val translate_const_exp : encoded_exp -> encoded_const

val translate : env -> full_contract_sig -> string -> datatype -> encoded_const
