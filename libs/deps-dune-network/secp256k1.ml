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

type context

module Privkey = struct

  type t = string (* 32 bytes *)
  let of_string s =
    assert (String.length s = 32);
    s
end

module ABSTRACT : sig

  type pubkey
  type signature
  type recoverable_signature

  val new_pubkey : unit -> pubkey
  val new_signature : unit -> signature
  val new_recoverable_signature : unit -> recoverable_signature

end = struct

  type pubkey = string
  type signature = string
  type recoverable_signature = string

  let new_pubkey() = String.make 64 '\000'
  let new_signature() = String.make 64 '\000'
  let new_recoverable_signature() = String.make 65 '\000'

end

include ABSTRACT

type create_flag = VERIFY | SIGN

external context_create : create_flag list -> context =
    "secp256k1_context_create_ml"

(* clone and destroy not implemented *)

external ec_pubkey_create :
  context ->
  pubkey -> (* Out *)
  Privkey.t -> (* In *)
  bool =
  "secp256k1_ec_pubkey_create_ml"

external pubkey_parse :
  context ->
  pubkey ->  (* Out *)
  string ->  (* In *)
  bool
  =
  "secp256k1_ec_pubkey_parse_ml"

type serial_flag = EC_COMPRESSED | EC_UNCOMPRESSED

external pubkey_serialize :
  context ->
  string ->   (* Out *)
  pubkey ->   (* In *)
  serial_flag list ->  (* In *)
  int         (* Used size of output *)
  =
  "secp256k1_ec_pubkey_serialize_ml"


external signature_parse_compact :
  context ->
  signature -> (* Out *)
  string ->    (* In 64 chars *)
  bool
  =
  "secp256k1_ecdsa_signature_parse_compact_ml"

external signature_parse_der :
  context ->
  signature -> (* Out *)
  string ->    (* In *)
  bool
  =
  "secp256k1_ecdsa_signature_parse_der_ml"

external signature_serialize_compact :
  context ->
  string ->      (* Out 64 chars *)
  signature ->   (* In *)
  unit
  =
  "secp256k1_ecdsa_signature_serialize_compact_ml"

external signature_serialize_der :
  context ->
  string ->      (* Out *)
  signature ->   (* In *)
  int
  =
  "secp256k1_ecdsa_signature_serialize_der_ml"

external verify :
  context ->
  signature ->    (* In *)
  string ->       (* In *)
  pubkey ->
  bool
  =
  "secp256k1_ecdsa_verify_ml"

external sign :
  context ->
  signature ->  (* Out *)
  string ->     (* In 32 bytes *)
  Privkey.t ->     (* In 32 bytes *)
  unit
  =
  "secp256k1_ecdsa_sign_ml"

module Context = struct
  type t = context

  let create = context_create
end

module Pubkey = struct
  type t = pubkey
  let parse ctx s =
    let sg = new_pubkey () in
    if pubkey_parse ctx sg s then
      Some sg
    else None
  let serialize ctx sg flags =
    let s = String.make 65 '\000' in
    let len = pubkey_serialize ctx s sg flags in
    String.sub s 0 len
  let create ctx seckey =
    let pubkey = new_pubkey () in
    if ec_pubkey_create ctx pubkey seckey then
      Some pubkey
    else None
end

module Signature = struct
  type t = signature

  let parse_compact ctx s =
    let sg = new_signature () in
    if signature_parse_compact ctx sg s then
      Some sg
    else None
  let parse_der ctx s =
    let sg = new_signature () in
    if signature_parse_der ctx sg s then
      Some sg
    else None

  let serialize_compact ctx sg =
    let s = String.make 64 '\000' in
    signature_serialize_compact ctx s sg;
    s

  let serialize_der ctx sg =
    let s = String.make 256 '\000' in
    let len = signature_serialize_der ctx s sg in
    String.sub s 0 len

end


external sign :
  context ->
  signature ->  (* Out *)
  string ->     (* In 32 bytes *)
  Privkey.t ->     (* In 32 bytes *)
  unit
  =
  "secp256k1_ecdsa_sign_ml"

let sign ctx s seckey =
  let sg = new_signature () in
  sign ctx sg s seckey;
  sg

module ECDSA_recoverable = struct

  type signature = recoverable_signature

  external signature_parse_compact :
    context ->
    recoverable_signature -> (* Out *)
    string ->    (* In: 64 byte compact signature input *)
    int ->       (* In: recovery id { 0,1,2,3 } *)
    bool
    =
    "secp256k1_ecdsa_recoverable_signature_parse_compact_ml"

  let signature_parse_compact ctx s id =
    let recsig = new_recoverable_signature () in
    if signature_parse_compact ctx recsig s id then
      Some recsig
    else None

  external signature_serialize_compact :
    context ->
    string ->    (* Out: 64 byte compact signature output *)
    recoverable_signature -> (* In *)
    int          (* Out: recovery id : 0,1,2,3 *)
    =
    "secp256k1_ecdsa_recoverable_signature_serialize_compact_ml"

  let signature_serialize_compact ctx recsig =
    let s = String.make 64 '\000' in
    let id = signature_serialize_compact ctx s recsig in
    (s, id)

  external signature_convert :
    context ->
    Signature.t -> (* Out *)
    recoverable_signature ->       (* In *)
    unit
    =
    "secp256k1_ecdsa_recoverable_signature_convert_ml"

  let signature_convert ctx recsig =
    let sg = new_signature () in
    signature_convert ctx sg recsig;
    sg

  external sign :
    context ->
    recoverable_signature ->       (* Out *)
    string ->          (* In: 32 bytes of data *)
    Privkey.t ->          (* In: seckey *)
    bool
    =
    "secp256k1_ecdsa_recoverable_sign_ml"

  let sign ctx s seckey =
    let recsig = new_recoverable_signature () in
    if sign ctx recsig s seckey then
      Some recsig
    else None

  external recover :
    context ->
    pubkey ->          (* Out *)
    recoverable_signature ->       (* In *)
    string ->          (* In: 32 bytes of data *)
    bool
    =
    "secp256k1_ecdsa_recoverable_recover_ml"

  let recover ctx recsig s =
    let pubkey = new_pubkey () in
    if recover ctx pubkey recsig s then
      Some pubkey
    else None

end
