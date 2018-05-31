(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val mic_of_tez : tez -> string
val mic_of_integer : integer -> Z.t

val tez_of_mic : string -> tez
val integer_of_mic : Z.t -> integer

val liq_of_tez : tez -> string
val liq_of_integer : integer -> string

val tez_of_liq : string -> tez
val integer_of_liq : string -> integer

val int_of_integer : integer -> int
val integer_of_int : int -> integer


module Liquid : sig
  val string_of_type : datatype -> string
  val string_of_const : const -> string
  val string_of_contract : ?debug:bool -> ('a, 'b) exp contract -> string
  val string_of_contract_types : ?debug:bool -> typed_contract -> string
  val string_of_code : ?debug:bool -> ('a, 'b) exp -> string
  val string_of_code_types : ?debug:bool -> typed_exp -> string
end

module Michelson : sig
  val string_of_type : datatype -> string
  val line_of_type : datatype -> string
  val string_of_const : const -> string
  val line_of_const : const -> string
  val string_of_contract : michelson_contract -> string
  val line_of_contract : michelson_contract -> string
  val string_of_code : michelson_exp -> string
  val line_of_code : michelson_exp -> string
  val string_of_loc_michelson : loc_michelson -> string
  val line_of_loc_michelson : loc_michelson -> string
end

val string_of_node : node -> string
