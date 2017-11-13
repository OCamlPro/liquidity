(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val error :
  location ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

val typecheck_contract :
  warnings:bool -> env -> syntax_contract -> typed_contract

                       (*
val uniq_ident : string -> string
                        *)

val typecheck_code :
  warnings:bool -> env ->
  syntax_contract ->
  LiquidTypes.datatype ->
  syntax_exp ->
  typed_exp

val check_const_type :
  ?from_mic:bool ->
  to_tez:(string -> LiquidTypes.tez) ->
  LiquidTypes.location ->
  LiquidTypes.datatype -> LiquidTypes.const -> LiquidTypes.const
