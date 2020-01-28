
type storage = bool

let%entry main
  (parameter : (string, tez) map)
  _storage =

  let amount = Current.amount() in
  let f ((_, v), acc) =
    if v + amount > 5.0tz then true
    else acc
  in

  let b = Map.fold f parameter false in
  ([], b)
