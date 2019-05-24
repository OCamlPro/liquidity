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

(* Code from BLAKE/BLAKE *)
external blake2b : (* out *)string -> (* in *)string -> (* key *)string -> int = "blake2b_ml"


let blake2b ?(key = "") input =
  let hash = String.make 64 '\000' in
  let n = blake2b hash input key in
  assert (n = 0);
  hash


external blake2b_size_of_context : unit -> int = "blake2b_size_of_context_ml"

type blake2b_ctx
external blake2b_init : string -> int -> blake2b_ctx = "blake2b_init_ml"
external blake2b_init_key : string -> int -> string -> blake2b_ctx = "blake2b_init_key_ml"
external blake2b_update : blake2b_ctx -> string -> unit = "blake2b_update_ml"
external blake2b_final : blake2b_ctx -> string -> unit = "blake2b_final_ml"


let blake2b_ctx_size = blake2b_size_of_context ()

let blake2b_init ?(size=64) () =
  let ctx = String.make blake2b_ctx_size '\000' in
  blake2b_init ctx size, String.make size '\000'

let blake2b_update (ctx,_) input = blake2b_update ctx input

let blake2b_final (ctx, hash) =
  blake2b_final ctx hash;
  hash

let blake2b input =
  let ctx = blake2b_init () in
  blake2b_update ctx input;
  blake2b_final ctx


(* Checks from Wikipedia :-) *)


let () =
  let test_BLAKE2b_512 input =
    String.uppercase_ascii (Hex_encode.hex_encode (blake2b input))
  in
  assert (test_BLAKE2b_512 "" =
            "786A02F742015903C6C6FD852552D272912F4740E15847618A86E217F71F5419\
             D25E1031AFEE585313896444934EB04B903A685B1448B755D56F701AFE9BE2CE");
  assert (test_BLAKE2b_512 "The quick brown fox jumps over the lazy dog" =
            "A8ADD4BDDDFD93E4877D2746E62817B116364A1FA7BC148D95090BC7333B3673\
             F82401CF7AA2E4CB1ECD90296E3F14CB5413F8ED77BE73045B13914CDCD6A918");
  ()




(* Code from PolarSSL *)
external sha256 : (* out *)string -> (* in *)string -> (* is224 *) bool -> unit = "sha256_ml"

let sha256 input =
  let hash = String.make 32 '\000' in
  sha256 hash input false;
  hash

external sha256_size_of_context : unit -> int = "sha256_size_of_context_ml"

type sha256_ctx
external sha256_init : string -> sha256_ctx = "sha256_init_ml"
external sha256_update : sha256_ctx -> string -> unit = "sha256_update_ml"
external sha256_final : sha256_ctx -> string -> unit = "sha256_final_ml"

let sha256_ctx_size = sha256_size_of_context () (* 108 normally *)

let sha256_init () =
  let ctx = String.make sha256_ctx_size '\000' in
  let new_ctx = sha256_init ctx in
  new_ctx

let sha256_final ctx =
  let hash = String.make 32 '\000' in
  sha256_final ctx hash;
  hash

let sha256 input =
  let ctx = sha256_init () in
  sha256_update ctx input;
  sha256_final ctx


let () =
  let test_SHA256 input = Hex_encode.hex_encode (sha256 input)
  in
  assert (test_SHA256 "" =
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");

  assert (test_SHA256 "The quick brown fox jumps over the lazy dog" =
            "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592");
  ()



external sha3_size_of_context : unit -> int = "sha3_size_of_context_ml"

type sha3_ctx
type sha3_kind =
  | KEC256
  | KEC384
  | KEC512

external sha3_init : string -> sha3_kind -> sha3_ctx = "sha3_init_ml"
external sha3_update : sha3_ctx -> string -> unit = "sha3_update_ml"
external sha3_final : sha3_ctx -> string -> unit = "sha3_final_ml"

let sha3_ctx_size = sha3_size_of_context () (* 108 normally *)

let sha3_init sha3_kind =
  let ctx = String.make sha3_ctx_size '\000' in
  let output = String.make (match sha3_kind with
                            | KEC256 -> 32
                            | KEC384 -> 48
                            | KEC512 -> 64) '\000' in
  let new_ctx = sha3_init ctx sha3_kind in
  new_ctx, output

let sha3_update (ctx,_) s = sha3_update ctx s

let sha3_final (ctx, output) =
  sha3_final ctx output;
  output

let sha3 sha3_kind input =
  let ctx = sha3_init sha3_kind in
  sha3_update ctx input;
  sha3_final ctx

module Hash = struct

  let digest = function
    | `SHA256 -> sha256
    | `SHA3_KEC -> sha3 KEC256
    | `SHA3_KEC512 -> sha3 KEC512
end


let () =
  let test_KEC input = Hex_encode.hex_encode (sha3 KEC256 input)
  in
  assert (test_KEC "" =
            "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");

  assert (test_KEC "The quick brown fox jumps over the lazy dog" =
            "4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15");

  ()
