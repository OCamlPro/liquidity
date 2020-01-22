

let f (x, _) = x

type storage = unit

let%entry default
      (parameter : int)
      storage =
  let storage = f (storage, parameter) in
  ( [], storage )
