type storage = key_hash

let%entry main (parameter : key) (storage : key_hash) =
   ([] : operation list), Crypto.hash_key parameter
