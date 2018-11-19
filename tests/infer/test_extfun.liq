
[%%version 0.4]

let f (x, _) = x

type storage = unit

let%entry main parameter storage =
  let storage = f (storage, parameter) in
  [], storage
