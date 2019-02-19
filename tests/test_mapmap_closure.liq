[%%version 0.5]
type storage = (string, bool) map

let%entry main
  (parameter : (string, tez) map)
  _storage =

  let amount = Current.amount() in
  let f (_, acc) =
    acc + amount > 5.0tz
  in

  let m = Map.map f parameter in
  ([], m)
