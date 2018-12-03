type storage = key_hash

let%entry main (k : key) _ =
  let h = Crypto.hash_key k in
  ([] : operation list), h
