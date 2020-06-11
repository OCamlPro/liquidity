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

module Public_key_hash = struct
  include Blake2B.MakeMulti(Base58)(struct
      let name = "Ed25519.Public_key_hash"
      let title = "An Ed25519 public key hash"
      let b58check_prefix = Base58.DunePrefix.ed25519_public_key_hash
      let alternate_prefixes = [ Base58.Prefix.ed25519_public_key_hash ]
      let size = Some 20
    end)
  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
  let of_public_key v =
    hash_bytes
      [ Sodium.Sign.Bigbytes.of_public_key v ]
end

module Public_key_hash_tezos = struct
  include Public_key_hash

  let to_b58check pkh = match to_b58check_all pkh with
    | _ :: x :: _ -> x
    | _ -> assert false

  let to_short_b58check s =
      String.sub
      (to_b58check s)
      0
      (10 + String.length (Base58.prefix b58check_encoding))
end


module Public_key_hash_dune = Public_key_hash


open Sodium

module Public_key = struct

  type t = Sign.public_key

  let size = Sign.public_key_size

  let name = "Ed25519.Public_key"
  let title = "Ed25519 public key"

  let to_bytes pk = Sign.Bigbytes.of_public_key pk

  let of_bytes_opt s =
    if MBytes.length s < size then None
    else
      try Some (Sign.Bigbytes.to_public_key s)
      with _ -> None

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let hash v =
    Public_key_hash.hash_bytes
      [ Sign.Bigbytes.of_public_key v ]

  include Compare.Make(struct
      type nonrec t = t
      let compare = Sign.compare_public_keys
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeB58(struct
      type nonrec t = t
      let name = name
      let b58check_encoding = b58check_encoding
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

module Secret_key = struct

  type t = Sign.secret_key

  let name = "Ed25519.Secret_key"
  let title = "An Ed25519 secret key"

  let size = Sodium.Sign.seed_size

  let to_bytes sk =
    Sign.Bigbytes.of_seed (Sign.secret_key_to_seed sk)

  (* Fabrice:  Sign.skbytes = 32. So the blit will fail only if
       s is smaller than 32. What the point of testing > 64 ? *)
  let of_bytes_opt s =
    if MBytes.length s > 64 then None
    else
      let sk = MBytes.create size in
      MBytes.blit s 0 sk 0 size ;
      let sk, _ = Sign.seed_keypair (Sign.Bigbytes.to_seed sk) in
      Some sk

  let to_string s = MBytes.to_string (to_bytes s)
  let of_string_opt s = of_bytes_opt (MBytes.of_string s)

  let to_public_key = Sign.secret_key_to_public_key

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_seed
      ~length: size
      ~to_raw: (fun sk -> MBytes.to_string (to_bytes sk))
      ~of_raw: (fun buf -> of_bytes_opt (MBytes.of_string buf))
      ~wrap: (fun sk -> Data sk)

  (* Legacy NaCl secret key encoding. Used to store both sk and pk. *)
  let secret_key_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.ed25519_secret_key
      ~length: Sign.secret_key_size
      ~to_raw: (fun sk ->
          Bytes.to_string (Sign.Bytes.of_secret_key sk))
      ~of_raw: (fun buf ->
          if String.length buf <> Sign.secret_key_size then None
          else
            Some (Sign.Bytes.to_secret_key (Bytes.of_string buf)))
      ~wrap: (fun x -> Data x)

  let of_b58check_opt s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> Some x
    | None -> Base58.simple_decode secret_key_encoding s
  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Pervasives.failwith "Unexpected data (%s)" name
  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        generic_error
          "Failed to read a b58check_encoding data (%s): %S"
          name s

  let to_b58check s = Base58.simple_encode b58check_encoding s
  let to_short_b58check s =
    String.sub
      (to_b58check s) 0
      (10 + String.length (Base58.prefix b58check_encoding))

  let () =
    Base58.check_encoded_prefix b58check_encoding "edsk" 54 ;
    Base58.check_encoded_prefix secret_key_encoding "edsk" 98

  include Compare.Make(struct
      type nonrec t = t
      let compare a b =
        MBytes.compare (Sign.Bigbytes.of_secret_key a) (Sign.Bigbytes.of_secret_key b)
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

type t = MBytes.t

type watermark = MBytes.t

let name = "Ed25519"
let title = "An Ed25519 signature"

let size = Sign.signature_size

let of_bytes_opt s =
  if MBytes.length s = size then Some s else None
let to_bytes x = x

let to_string s = MBytes.to_string (to_bytes s)
let of_string_opt s = of_bytes_opt (MBytes.of_string s)

type Base58.data +=
  | Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.ed25519_signature
    ~length: size
    ~to_raw: MBytes.to_string
    ~of_raw: (fun s -> Some (MBytes.of_string s))
    ~wrap: (fun x -> Data x)

let () =
  Base58.check_encoded_prefix b58check_encoding "edsig" 99

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Helpers.MakeB58(struct
    type nonrec t = t
    let name = name
    let b58check_encoding = b58check_encoding
  end)

include Helpers.MakeEncoder(struct
    type nonrec t = t
    let name = name
    let title = title
    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)
    let of_b58check = of_b58check
    let of_b58check_opt = of_b58check_opt
    let of_b58check_exn = of_b58check_exn
    let to_b58check = to_b58check
    let to_short_b58check = to_short_b58check
  end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let zero = MBytes.make size '\000'

let sign ?watermark sk msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  (Sign.Bigbytes.sign_detached sk msg)
  |> Sign.Bigbytes.of_signature

let check ?watermark pk signature msg =
  let msg =
    Blake2B.to_bytes @@
    Blake2B.hash_bytes @@
    match watermark with
    | None -> [msg]
    | Some prefix -> [ prefix ; msg ] in
  try
    Sign.Bigbytes.(verify pk (to_signature signature) msg) ;
    true
  with _ -> false

let generate_key ?seed () =
  match seed with
  | None ->
      let sk, pk = Sign.random_keypair () in
      Public_key.hash pk, pk, sk
  | Some seed ->
      let seedlen = MBytes.length seed in
      if seedlen < Sign.seed_size then
        invalid_arg (Printf.sprintf "Ed25519.generate_key: seed must \
                                     be at least %d bytes long (got %d)"
                       Sign.seed_size seedlen) ;
      let sk, pk = Sign.seed_keypair (Sign.Bigbytes.to_seed seed) in
      Public_key.hash pk, pk, sk


let deterministic_nonce sk msg =
  Hacl.Hash.SHA256.HMAC.digest ~key: (Secret_key.to_bytes sk) ~msg

let deterministic_nonce_hash sk msg =
  Blake2B.to_bytes (Blake2B.hash_bytes [deterministic_nonce sk msg])


include Compare.Make(struct
    type nonrec t = t
    let compare = MBytes.compare
  end)

let sk_of_bytes_opt = Secret_key.of_bytes_opt
