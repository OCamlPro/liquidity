(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val clean_code : loc_michelson -> loc_michelson
val clean_contract : loc_michelson_contract -> loc_michelson_contract
(* val remove_loc_contract : loc_michelson contract -> noloc_michelson contract *)
