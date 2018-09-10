(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Pretty-printing of Liquidity and Michelson, code and values *)

open LiquidTypes

(** {2 Helper conversion function } *)

(** Convert mutez to a Michelson mutez constant *)
val mic_mutez_of_tez : tez -> Z.t
(** Convert integer to a Michelson integer constant *)
val mic_of_integer : integer -> Z.t

(** Convert mutez to a Michelson mutez constant *)
val tez_of_mic_mutez : Z.t -> tez
(** Convert Michelson integer to Liquidity integer *)
val integer_of_mic : Z.t -> integer

(** Pretty print Liquidity tez constant *)
val liq_of_tez : tez -> string
(** Pretty print integer constant *)
val liq_of_integer : integer -> string

(** Parse liquidity tez constant *)
val tez_of_liq : string -> tez
(** Parse liquidity integer constant *)
val integer_of_liq : string -> integer

(** Convert OCaml liquidity integer to 31 bits integer *)
val int_of_integer : integer -> int
(** Convert OCaml 31 bits integer to liquidity integer *)
val integer_of_int : int -> integer


(** {2 Pretty-printing of Liquidity } *)

module Liquid : sig
  (** Pretty-print Liquidity type *)
  val string_of_type : datatype -> string
  (** Pretty-print Liquidity constant *)
  val string_of_const : const -> string
  (** Pretty-print Liquidity contract *)
  val string_of_contract : ?debug:bool -> ('a, 'b) exp contract -> string
  (** Pretty-print Liquidity typed contract *)
  val string_of_contract_types : ?debug:bool -> typed_contract -> string
  (** Pretty-print Liquidity code *)
  val string_of_code : ?debug:bool -> ('a, 'b) exp -> string
  (** Pretty-print Liquidity typed code *)
  val string_of_code_types : ?debug:bool -> typed_exp -> string
end

(** {2 Pretty-printing of Michelson } *)

module Michelson : sig
  (** Pretty-print Michelson type *)
  val string_of_type : datatype -> string
  (** Pretty-print Michelson type on a single line *)
  val line_of_type : datatype -> string
  (** Pretty-print Michelson constant *)
  val string_of_const : const -> string
  (** Pretty-print Michelson constant on a single line *)
  val line_of_const : const -> string
  (** Pretty-print intermediate Michelson contract *)
  val string_of_contract : michelson_contract -> string
  (** Pretty-print intermediate Michelson contract on a single line *)
  val line_of_contract : michelson_contract -> string
  (** Pretty-print intermediate Michelson code *)
  val string_of_code : michelson_exp -> string
  (** Pretty-print intermediate Michelson code on a single line *)
  val line_of_code : michelson_exp -> string
  (** Pretty-print actual Michelson contract *)
  val string_of_loc_michelson : loc_michelson -> string
  (** Pretty-print actual Michelson contract on a single line *)
  val line_of_loc_michelson : loc_michelson -> string
end

(** Print a decompiled node (for debugging purposes) *)
val string_of_node : node -> string
