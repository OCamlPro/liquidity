type storage = tez

let%entry main ( v : nat ) _ =
  (* conversion from nat to tez *)
  let amount = v * 1tz in
  ([] : operation list), amount
