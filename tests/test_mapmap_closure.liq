[%%version 0.2]
let%entry main
  (parameter : (string, tez) map)
  (storage : (string, bool) map) =

  let amount = Current.amount() in
  let f (arg: (string * tez)) =
    arg.(1) + amount > 5.0tz
  in

  let m = Map.map f parameter in
  (([] : operation list), m)
