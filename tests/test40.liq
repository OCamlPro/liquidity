type storage = key_hash

let%entry main (parameter : key) _storage =
   [], Crypto.hash_key parameter
