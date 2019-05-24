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

module Generichash = struct
  let primitive = "blake2b"
  type hash = string
  let compare = compare

  let init ~size () =
    Nocrypto.blake2b_init ~size ()
  let final = Nocrypto.blake2b_final


  module Bytes = struct
    let hash = Nocrypto.blake2b
    let to_hash s = assert false
    let of_hash s = assert false
    let update = Nocrypto.blake2b_update
  end

  module Bigbytes = Bytes
end

module Sign = struct
  let signature_size = 64
  let public_key_size = 32
  let secret_key_size = 32

  module Bytes = struct
    let of_public_key s = assert false
    let to_public_key s = assert false
    let of_secret_key s = assert false
    let to_secret_key s = assert false
    let of_signature s = assert false
    let to_signature s = assert false
    let sign_detached _secret_key _msg = assert false
    let verify _public_key _signature _msg = assert false
  end
  module Bigbytes = Bytes
  type public_key = string
  let compare_public_keys _ _ = assert false
  type secret_key = string
  let secret_key_to_public_key _ = assert false
end
