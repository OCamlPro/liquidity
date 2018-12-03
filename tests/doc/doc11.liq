type storage = nat

let%entry main ( v : tez ) _ =
  (* conversion from tez to nat *)
  let (nat, rem_tez) = match v / 1tz with
    | Some qr -> qr
    | None -> failwith "division by 0 impossible" in
  ([] : operation list), nat
