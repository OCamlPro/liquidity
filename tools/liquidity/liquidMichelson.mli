(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Michelson code generation *)

open LiquidTypes

(** Compile contract to intermediate michelson representation *)
val translate : encoded_contract -> loc_michelson_contract

val compile_const : encoded_const -> loc_michelson const
