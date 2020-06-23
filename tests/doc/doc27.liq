type storage = key_hash

let%entry default (k : key) _ =
  let h = Crypto.hash_key k in
  [], h
