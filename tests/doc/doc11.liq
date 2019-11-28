type storage = nat

let%entry main ( v : dun ) _ =
  (* conversion from dun to nat *)
  let (nat, rem_dun) = match v / 1DUN with
    | Some qr -> qr
    | None -> failwith "division by 0 impossible" in
  [], nat
