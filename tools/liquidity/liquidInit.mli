(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

type init =
  | Init_constant of LiquidTypes.const
  | Init_code of (LiquidTypes.syntax_contract *
                  LiquidTypes.loc_michelson_contract)

val compile_liquid_init : env -> contract_sig -> datatype -> syntax_init -> init
