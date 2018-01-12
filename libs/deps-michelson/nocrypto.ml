module Hash = struct

  let digest = function
    | `SHA256 ->
      fun s ->
        Cstruct.to_bytes s
        |> Sodium.Hash.Bytes.Sha256.digest
        |> Sodium.Hash.Bytes.Sha256.of_hash
        |> Cstruct.of_bytes
    | `SHA3_KEC -> fun _ -> failwith "sha3_kec not implemented"
    | `SHA3_KEC512 -> fun _ -> failwith "sha3_kec512 not implemented"

end


(* type blake2b_ctx
 *
 * let blake2b_init ?(size=64) () =
 *   Obj.magic ("Nocrypto.blake2b_init not implemented")
 *
 * let blake2b_update (ctx,_) input =
 *   failwith ("Nocrypto.blake2b_update not implemented")
 *
 * let blake2b_final (ctx, hash) =
 *   Obj.magic ("Nocrypto.blake2b_final not implemented")
 *
 * let blake2b input =
 *   failwith ("Nocrypto.blake2b not implemented") *)
