
type storage = bool

let%entry default
  (parameter : tez set)
  _storage =

  let amount = Current.amount() in
  let f (x, acc) =
    if x + amount > 5.0tz then true
    else acc
  in

  let b = Set.fold f parameter false in
  ([], b)
