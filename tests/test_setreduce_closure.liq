let%entry main
  (parameter : tez set)
  (storage : bool) =

  let amount = Current.amount() in
  let f (arg: tez * bool) =
    if arg.(0) + amount > 5.0tz then true
    else arg.(1)
  in

  let b = Set.fold f parameter false in
  (([] : operation list), b)
