(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val untype_contract : (datatype, 'a) exp contract -> (datatype, 'b) exp contract
val untype_code : (datatype, 'a) exp -> (datatype, 'b) exp
