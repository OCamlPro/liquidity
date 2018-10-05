(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Compute free variables of an expression *)
val bv : ('a, 'b) exp -> StringSet.t

(** Add free variable information to a contract *)
val bound_contract : ('a, 'b) exp contract -> ('a, 'b) exp contract

(** Add free variable information to an expression  *)
val bound : ('a, 'b) exp -> ('a, 'b) exp
