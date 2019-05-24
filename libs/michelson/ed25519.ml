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

open Error_monad

  module Public_key_hash = Blake2B.Make(Base58)(struct
      let name = "Ed25519.Public_key_hash"
      let title = "An Ed25519 public key ID"
      let b58check_prefix = Base58.Prefix.ed25519_public_key_hash
      let size = Some 20
    end)

  (* let () =
   *   Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36 *)

  module Public_key = struct

    type t = Sodium.Sign.public_key
    let compare = Sodium.Sign.compare_public_keys
    let (=) xs ys = compare xs ys = 0
    let (<>) xs ys = compare xs ys <> 0
    let (<) xs ys = compare xs ys < 0
    let (<=) xs ys = compare xs ys <= 0
    let (>=) xs ys = compare xs ys >= 0
    let (>) xs ys = compare xs ys > 0
    let max x y = if x >= y then x else y
    let min x y = if x <= y then x else y

    type Base58.data +=
      | Public_key of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_public_key
        ~length:Sodium.Sign.public_key_size
        ~to_raw:(fun x -> Bytes.to_string (Sodium.Sign.Bytes.of_public_key x))
        ~of_raw:(fun x ->
            try Some (Sodium.Sign.Bytes.to_public_key (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Public_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 public key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = Sodium.Sign.Bytes.to_public_key s

    (* let param ?(name="ed25519-public") ?(desc="Ed25519 public key (b58check-encoded)") t =
     *   Cli_entries.(param ~name ~desc (parameter (fun _ str -> Lwt.return (of_b58check str))) t) *)

    (* let () =
     *   Base58.check_encoded_prefix b58check_encoding "edpk" 54 *)

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
           (conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 public key: unexpected prefix.")
             string)
        ~binary:
          (conv
             Sodium.Sign.Bigbytes.of_public_key
             Sodium.Sign.Bigbytes.to_public_key
             (Fixed.bytes Sodium.Sign.public_key_size))

    let hash v =
      Public_key_hash.hash_bytes
        [ Sodium.Sign.Bigbytes.of_public_key v ]

  end

  module Secret_key = struct

    type t = Sodium.Sign.secret_key

    type Base58.data +=
      | Secret_key of t

    let to_bytes s =
      Sodium.Sign.Bytes.of_seed (Sodium.Sign.secret_key_to_seed s)
    let of_bytes s =
      let l = Bytes.length s in
      if l = Sodium.Sign.seed_size then
        let sk, _ = Sodium.Sign.seed_keypair (Sodium.Sign.Bytes.to_seed s) in
        sk
      else if l = Sodium.Sign.secret_key_size then
        Sodium.Sign.Bytes.to_secret_key s
      else
        Pervasives.failwith "Secret_key.of_bytes: Wrong size for secret key"

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_seed
        ~length:Sodium.Sign.seed_size
        ~to_raw:(fun x -> Bytes.to_string (to_bytes x))
        ~of_raw:(fun x ->
            try Some (of_bytes (Bytes.of_string x))
            with _ -> None)
        ~wrap:(fun x -> Secret_key x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 secret key)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s


    (* let param ?(name="ed25519-secret") ?(desc="Ed25519 secret key (b58check-encoded)") t =
     *   Cli_entries.(param ~name ~desc (parameter (fun _ str -> Lwt.return (of_b58check str))) t) *)

    (* let () =
     *   Base58.check_encoded_prefix b58check_encoding "edsk" 98 *)

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 secret key: unexpected prefix.")
             string)
        ~binary:
          (conv
             Sodium.Sign.Bigbytes.of_secret_key
             Sodium.Sign.Bigbytes.to_secret_key
             (Fixed.bytes Sodium.Sign.secret_key_size))

  end

  let sign key msg =
    Sodium.Sign.Bigbytes.(of_signature @@ sign_detached key msg)

  module Signature = struct

    type t = MBytes.t

    type Base58.data +=
      | Signature of t

    let b58check_encoding =
      Base58.register_encoding
        ~prefix: Base58.Prefix.ed25519_signature
        ~length:Sodium.Sign.signature_size
        ~to_raw:MBytes.to_string
        ~of_raw:(fun s -> Some (MBytes.of_string s))
        ~wrap:(fun x -> Signature x)

    let of_b58check_opt s = Base58.simple_decode b58check_encoding s
    let of_b58check_exn s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> x
      | None -> Pervasives.failwith "Unexpected hash (ed25519 signature)"
    let of_b58check s =
      match Base58.simple_decode b58check_encoding s with
      | Some x -> Ok x
      | None -> generic_error "Unexpected hash (ed25519 public key)"
    let to_b58check s = Base58.simple_encode b58check_encoding s

    let of_bytes s = MBytes.of_string (Bytes.to_string s)

    (* let param ?(name="signature") ?(desc="Signature (b58check-encoded)") t =
     *   Cli_entries.(param ~name ~desc (parameter (fun _ str -> Lwt.return (of_b58check str))) t) *)

    (* let () =
     *   Base58.check_encoded_prefix b58check_encoding "edsig" 99 *)

    let encoding =
      let open Data_encoding in
      splitted
        ~json:
          (conv
             (fun s -> Base58.simple_encode b58check_encoding s)
             (fun s ->
                match Base58.simple_decode b58check_encoding s with
                | Some x -> x
                | None -> Data_encoding.Json.cannot_destruct
                            "Ed25519 signature: unexpected prefix.")
             string)
        ~binary: (Fixed.bytes Sodium.Sign.signature_size)

    let check public_key signature msg =
      try
        Sodium.Sign.Bigbytes.(verify public_key (to_signature signature) msg) ;
        true
      with _ -> false

  end


let generate_key () =
  let secret, pub = Sodium.Sign.random_keypair () in
  (Public_key.hash pub, pub, secret)
