[%%version 0.5]

type storage = int

let rec f x =
    if x > 0 then f (x - 1)
    else x

let%entry test_rec
    (parameter : int)
    _storage =

  let storage = f parameter in
  let storage = f storage in

  ( [], storage )
