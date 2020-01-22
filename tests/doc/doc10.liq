type storage = dun

let%entry default ( v : nat ) _ =
  (* conversion from nat to dun *)
  let amount = v * 1DUN in
  [], amount
