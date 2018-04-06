let%entry main (parameter : key) (storage : unit) : key_hash * unit =
    (Crypto.hash_key parameter, ())
