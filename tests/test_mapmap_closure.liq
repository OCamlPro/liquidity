[%%version 0.12]
let%entry main
  (parameter : string)
  (storage : (string, tez) map)
  : (string, bool) map * (string, tez) map =

  let amount = Current.amount() in
  let f (arg: (string * tez)) =
    arg.(1) + amount > 5.0tz
  in

  let m = Map.map f storage in
  (m, storage)
