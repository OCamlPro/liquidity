
[%%version 0.4]

type storage = int

let%entry main
      (parameter : int)
      storage =
  let x = parameter + 10 in
  let f arg y =
    arg.(0) + x + y
  in
  let storage = f (parameter, storage) 199 in
  ( [], storage )
