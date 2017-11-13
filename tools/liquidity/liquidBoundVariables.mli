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
val bound_contract : ('a, 'b) exp contract -> ('a, 'b) exp contract
val bound : ('a, 'b) exp -> ('a, 'b) exp
val bv : ('a, 'b) exp -> StringSet.t
