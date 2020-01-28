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

type create_flag = VERIFY | SIGN
type serial_flag = EC_COMPRESSED | EC_UNCOMPRESSED

module Context : sig
  type t

  val create : create_flag list -> t
end

module Privkey : sig
  type t
  val of_string : string -> t
end

module Pubkey : sig
  type t
  (* This function supports parsing compressed (33 bytes,
   * header byte 0x02 or 0x03), uncompressed (65 bytes, header byte
   0x04), or hybrid (65 bytes, header byte 0x06 or 0x07) format public
   keys. *)
  val parse : Context.t -> string(*[65]*) -> t option
  val serialize : Context.t -> t -> serial_flag list -> string
  val create : Context.t -> Privkey.t -> t option
end

module Signature : sig
  type t
  (* The signature must consist of a 32-byte big endian R value,
   followed by a 32-byte big endian S value. If R or S fall outside of
   [0..order-1], the encoding is invalid. R and S with value 0 are
   allowed in the encoding.
   *)
  val parse_compact : Context.t -> string(*[64]*) -> t option

  (* This function will accept any valid DER encoded signature, even
   if the encoded numbers are out of range.  *)
  val parse_der : Context.t -> string -> t option
  val serialize_compact : Context.t -> t -> string(*[64]*)
  val serialize_der : Context.t -> t -> string

end

val verify : Context.t -> Signature.t -> string(*[32]*) -> Pubkey.t -> bool
val sign : Context.t -> string(*[32]*) -> Privkey.t -> Signature.t

module ECDSA_recoverable :
  sig

    type signature

    val signature_parse_compact :
      Context.t -> string(*[64]*) -> int -> signature option

    val signature_serialize_compact :
      Context.t -> signature -> string(*[64]*) * int

    val signature_convert :
      Context.t -> signature -> Signature.t

    val sign :
      Context.t -> string(*[32]*) -> Privkey.t -> signature option

    val recover :
      Context.t -> signature -> string(*[32]*) -> Pubkey.t option

  end
