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

type t = bytes

val create: int -> t

val length: t -> int

val compare : t -> t -> int

val copy: t -> t

val sub: t -> int -> int -> t
(** [sub src ofs len] extract a sub-array of [src] starting at [ofs]
    and of length [len]. No copying of elements is involved: the
    sub-array and the original array share the same storage space. *)

val blit: t -> int -> t -> int -> int -> unit
(** [blit src ofs_src dst ofs_dst len] copy [len] bytes from [src]
    starting at [ofs_src] into [dst] starting at [ofs_dst].] *)

val blit_from_string: string -> int -> t -> int -> int -> unit
(** See [blit] *)

val of_string: string -> t
(** [of_string s] create an byte array filled with the same content than [s]. *)

val to_string: t -> string
(** [to_string b] dump the array content in a [string]. *)

val substring: t -> int -> int -> string
(** [substring b ofs len] is equivalent to [to_string (sub b ofs len)]. *)

(** Functions reading and writing bytes  *)

val get_char: t -> int -> char
(** [get_char buff i] reads 1 byte at offset i as a char *)


val get_uint8: t -> int -> int
(** [get_uint8 buff i] reads 1 byte at offset i as an unsigned int of 8
    bits. i.e. It returns a value between 0 and 2^8-1 *)

val get_int8: t -> int -> int
(** [get_int8 buff i] reads 1 byte at offset i as a signed int of 8
    bits. i.e. It returns a value between -2^7 and 2^7-1 *)

val set_char: t -> int -> char -> unit
(** [set_char buff i v] writes [v] to [buff] at offset [i] *)

val set_int8: t -> int -> int -> unit
(** [set_int8 buff i v] writes the least significant 8 bits of [v]
    to [buff] at offset [i] *)


val get_uint16: t -> int -> int
(** [get_uint16 buff i] reads 2 bytes at offset i as an unsigned int
      of 16 bits. i.e. It returns a value between 0 and 2^16-1 *)

val get_int16: t -> int -> int
(** [get_int16 buff i] reads 2 byte at offset i as a signed int of
      16 bits. i.e. It returns a value between -2^15 and 2^15-1 *)

val get_int32: t -> int -> int32
(** [get_int32 buff i] reads 4 bytes at offset i as an int32. *)

val get_int64: t -> int -> int64
(** [get_int64 buff i] reads 8 bytes at offset i as an int64. *)

val get_double: t -> int -> float
(** [get_float buff i] reads 8 bytes at offset i as an IEEE754 double. *)

val set_int16: t -> int -> int -> unit
(** [set_int16 buff i v] writes the least significant 16 bits of [v]
      to [buff] at offset [i] *)

val set_int32: t -> int -> int32 -> unit
(** [set_int32 buff i v] writes [v] to [buff] at offset [i] *)

val set_int64: t -> int -> int64 -> unit
(** [set_int64 buff i v] writes [v] to [buff] at offset [i] *)


val set_double: t -> int -> float -> unit
(** [set_double buff i v] writes [v] to [buff] at offset [i] *)

val concat: t -> t -> t
