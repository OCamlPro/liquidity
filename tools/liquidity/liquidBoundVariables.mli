(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(* Compute free variables of an expression *)
val bound_contract : 'a exp contract -> 'a exp contract
val bound : 'a exp -> 'a exp
val bv : 'a exp -> StringSet.t
