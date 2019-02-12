(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
