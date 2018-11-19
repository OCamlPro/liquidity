[%%version 0.4]
type storage = (string, bool) map

let%entry main parameter storage =

  let amount = Current.amount() in
  let f arg =
    arg.(1) + amount > 5.0tz
  in

  let m = Map.map f parameter in
  [], m
