(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Peephole optimizations: try to simplify Michelson with peepholes
    optims.

    For now, mostly, move [DIP_DROP]s backwards to decrease the size of
    the stack. *)

open LiquidTypes

val simplify : loc_michelson_contract -> loc_michelson_contract
