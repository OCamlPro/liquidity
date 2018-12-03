type storage = nat

let%entry main ( x : int ) _ =
  (* conversion from int to nat *)
  let n = match%nat x with
    | Plus n -> n
    | Minus _ -> failwith "x shound not be negative" in
  ([] : operation list), n
