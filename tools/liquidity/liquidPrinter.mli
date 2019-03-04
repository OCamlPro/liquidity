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


module Syntax : sig
  val string_of_structure :
    Parsetree.structure -> (string * Location.t) list -> string
  val string_of_expression : Parsetree.expression -> string
  val string_of_core_type : Parsetree.core_type -> string
end

(** {2 Pretty-printing of Liquidity} *)

module Liquid : sig
  (** Pretty-print Liquidity type *)
  val string_of_type : datatype -> string
  (** Pretty-print Liquidity constant *)
  val string_of_const : (datatype, 'a) exp const -> string
  (** Pretty-print Liquidity contract *)
  val string_of_contract : typed_contract -> string
  (** Pretty-print typed Liquidity code *)
  val string_of_code : (datatype, 'a) exp -> string
end

(** {2 Pretty-printing of Liquidity terms for debugging} *)

module LiquidDebug : sig
  (** Pretty-print Liquidity type *)
  val string_of_type : datatype -> string
  (** Pretty-print Liquidity constant *)
  val string_of_const : ('a, 'b) exp const -> string
  (** Pretty-print Liquidity contract *)
  val string_of_contract : ?debug:bool -> ('a, 'b) exp contract -> string
  (** Pretty-print Liquidity typed contract *)
  val string_of_contract_types : ?debug:bool -> typed_contract -> string
  (** Pretty-print typed Liquidity code *)
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
  val string_of_const : michelson_exp const -> string
  (** Pretty-print Michelson constant on a single line *)
  val line_of_const : michelson_exp const -> string
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
