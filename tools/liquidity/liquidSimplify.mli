(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Simplify a Liquidity contract, mostly inlining and some simple
    simplifications. *)
val simplify_contract :
  ?decompile_annoted:bool ->
  encoded_contract -> encoded_exp StringMap.t -> encoded_contract
