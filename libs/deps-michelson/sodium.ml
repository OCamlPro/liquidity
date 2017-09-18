(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Generichash = struct
  let primitive = "blake2b"
  type hash = string
  let compare = compare

  let init ~size () =
    Nocrypto.blake2b_init ~size ()
  let final = Nocrypto.blake2b_final


  module Bytes = struct
    let hash = Nocrypto.blake2b
    let to_hash s = s
    let of_hash s = s
    let update = Nocrypto.blake2b_update
  end

  module Bigbytes = Bytes
end

module Sign = struct
  let signature_size = 64
  let public_key_size = 32
  let secret_key_size = 32

  module Bytes = struct
    let of_public_key s = s
    let to_public_key s = s
    let of_secret_key s = s
    let to_secret_key s = s
    let of_signature s = s
    let to_signature s = s
    let sign_detached _secret_key _msg = assert false
    let verify _public_key _signature _msg = assert false
  end
  module Bigbytes = Bytes
  type public_key = string
  let compare_public_keys _ _ = assert false
  type secret_key = string
end
