(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

(** Tez constants are stored with strings *)
type tez = { tezzies : string; mutez : string option }

(** Unbounded integer constants *)
type integer = { integer : Z.t }

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
