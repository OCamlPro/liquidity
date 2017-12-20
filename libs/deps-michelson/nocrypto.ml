module Hash = struct

  let digest = function
      `SHA256 | `SHA3_KEC | `SHA3_KEC512 ->
      fun _ -> Cstruct.of_string "Nocrypto.Hash.digest not implemented"

end

type blake2b_ctx

let blake2b_init ?(size=64) () =
  Obj.magic ("Nocrypto.blake2b_init not implemented")

let blake2b_update (ctx,_) input =
  failwith ("Nocrypto.blake2b_update not implemented")

let blake2b_final (ctx, hash) =
  Obj.magic ("Nocrypto.blake2b_final not implemented")

let blake2b input =
  failwith ("Nocrypto.blake2b not implemented")
