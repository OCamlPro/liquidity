(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception Missing_program_field of string

val convert_contract :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.contract ->
  LiquidTypes.loc_michelson LiquidTypes.contract * bool (* true if tz annoted *)

val contract_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidTezosTypes.contract * LiquidTezosTypes.env) option
