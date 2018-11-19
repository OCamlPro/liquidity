
type storage = bool

let%entry main parameter storage =

  let amount = Current.amount() in
  let f arg =
    if arg.(0).(1) + amount > 5.0tz then true
    else arg.(1)
  in

  let b = Map.fold f parameter false in
  [], b
