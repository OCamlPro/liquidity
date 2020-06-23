

type storage = int

let%entry default
      (parameter : int)
      storage =
  let _x = parameter + 10 in
  let f _arg _y =
    0
  in
  let storage = f (parameter, storage) 199 in
  ( [], storage )
