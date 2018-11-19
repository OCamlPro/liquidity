[%%version 0.4]

type storage = int

let rec f (x:'a) =
    if x > 0 then f (x - 1)
    else x

let%entry test_rec parameter _storage =

  let storage = f parameter in
  let storage = f storage in

  [], storage
