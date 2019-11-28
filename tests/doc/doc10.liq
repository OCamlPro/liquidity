type storage = dun

let%entry main ( v : nat ) _ =
  (* conversion from nat to dun *)
  let amount = v * 1DUN in
  [], amount
