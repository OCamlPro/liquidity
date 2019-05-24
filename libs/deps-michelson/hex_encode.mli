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

(** Tezos Utility library - Hexadecimal encoding *)

(** Parses a sequence of hexadecimal characters pairs as bytes *)
val hex_of_bytes: MBytes.t -> string

(** Prints a sequence of bytes as hexadecimal characters pairs *)
val bytes_of_hex: string -> MBytes.t

(** Interprets a sequence of hexadecimal characters pairs representing
    bytes as the characters codes of an OCaml string. *)
val hex_decode: string -> string

(** Formats the codes of the characters of an OCaml string as a
    sequence of hexadecimal character pairs. *)
val hex_encode: string -> string
