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

(* Bindings to https://github.com/dcposch/blakejs *)

(* Same interface as Tezos' Blake2 *)
module Blake2b : sig
  type t
  type hash = Hash of Bigstring.t

  val init : ?key:Bigstring.t -> int -> t

  val update : t -> Bigstring.t -> unit

  val final : t -> hash

  val direct : ?key:Bigstring.t -> Bigstring.t -> int -> hash
end = struct

  type t
  type hash = Hash of Bigstring.t

  let bytes_of_typed_array u =
    let l = u##length in
    let b = Bytes.create l in
    for i = 0 to l - 1 do
      Bytes.unsafe_set b i (Char.unsafe_chr (Typed_array.unsafe_get u i))
    done;
    b

  let string_of_typed_array u =
    Bytes.unsafe_to_string (bytes_of_typed_array u)

  let typed_array_of_bytes b =
    let l = Bytes.length b in
    let u = jsnew Typed_array.uint8Array (l) in
    Bytes.iteri (fun i c ->
        Typed_array.set u i (Char.code c)
      ) b;
    u

  let typed_array_of_string s =
    typed_array_of_bytes (Bytes.unsafe_of_string s)

  let blakejs = Js.Unsafe.global##blakejs

  let init ?key (size : int) : t =
    match key with
    | Some key ->
      Js.Unsafe.(fun_call blakejs##blake2bInit [|
          inject size;
          inject (Js.string (Bigstring.to_string key));
        |])
    | None ->
      Js.Unsafe.(fun_call blakejs##blake2bInit [|
          inject size
        |])

  let update (t : t)  (input : Bigstring.t) : unit =
    Js.Unsafe.(fun_call blakejs##blake2bUpdate [|
        inject t;
        Bigstring.to_string input |> typed_array_of_string |> inject;
      |])

  let final (t : t) : hash = Hash (Js.Unsafe.(fun_call blakejs##blake2bFinal [|
      inject t;
    |]) |> string_of_typed_array |> Bigstring.of_string)

  let direct ?key input size =
  let t = init ?key size in
  update t input;
  final t

end

(* let () =
 *   let test_BLAKE2b_512 input =
 *     let Blake2b.Hash h = Blake2b.direct (Bigstring.of_string input) 64 in
 *     String.uppercase_ascii (Hex.show (Hex.of_string (Bigstring.to_string h)))
 *   in
 *   assert (test_BLAKE2b_512 "" =
 *             "786A02F742015903C6C6FD852552D272912F4740E15847618A86E217F71F5419\
 *              D25E1031AFEE585313896444934EB04B903A685B1448B755D56F701AFE9BE2CE");
 *   assert (test_BLAKE2b_512 "The quick brown fox jumps over the lazy dog" =
 *             "A8ADD4BDDDFD93E4877D2746E62817B116364A1FA7BC148D95090BC7333B3673\
 *              F82401CF7AA2E4CB1ECD90296E3F14CB5413F8ED77BE73045B13914CDCD6A918");
 *   () *)
