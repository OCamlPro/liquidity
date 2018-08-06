(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

exception Missing_program_field of string

val convert_const_type :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.expr ->
  LiquidTypes.datatype ->
  LiquidTypes.const

val convert_const_notype :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.expr ->
  LiquidTypes.const

val convert_contract :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.contract ->
  LiquidTypes.loc_michelson_contract
  * bool (* true if tz annoted *)
  * (LiquidTypes.datatype, string) Hashtbl.t

val contract_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidTezosTypes.contract * LiquidTezosTypes.env) option

val const_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidTezosTypes.expr * LiquidTezosTypes.env) option
