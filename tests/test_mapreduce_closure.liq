let%entry main
  (parameter : (string, tez) map)
  (storage : bool) =

  let amount = Current.amount() in
  let f (arg: (string * tez) * bool) =
    if arg.(0).(1) + amount > 5.0tz then true
    else arg.(1)
  in

  let b = Map.reduce f parameter false in
  (([] : operation list), b)
