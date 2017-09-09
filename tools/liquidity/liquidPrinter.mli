(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

module Liquid : sig
  val string_of_type : LiquidTypes.datatype -> string
  val string_of_const : LiquidTypes.const -> string
  val string_of_contract : ?debug:bool ->
           'a LiquidTypes.exp LiquidTypes.contract -> string
  val string_of_code : ?debug:bool -> 'a LiquidTypes.exp -> string
end

module Michelson : sig
  val string_of_type : LiquidTypes.datatype -> string
  val string_of_const : LiquidTypes.const -> string
  val string_of_contract :
    LiquidTypes.michelson_exp LiquidTypes.contract -> string
  val string_of_code : LiquidTypes.michelson_exp -> string

  end

val string_of_node : LiquidTypes.node -> string
