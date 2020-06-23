type storage = key_hash

let%entry default (parameter : key) _storage =
   [], Crypto.hash_key parameter
