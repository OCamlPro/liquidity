
[%%version 0.5]

type storage = int

let%entry main
      (parameter : int)
      storage =
  let x = parameter + 10 in
  let f (arg, _) y =
    arg + x + y
  in
  let storage = f (parameter, storage) 199 in
  ( [], storage )
