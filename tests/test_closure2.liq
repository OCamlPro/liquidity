
[%%version 0.13]

let%entry main
      (parameter : int)
      (storage : int)
      : unit * int =
  let x = parameter + 10 in
  let f ( arg : int * int ) (y : int) =
    arg.(0) + x + y
  in
  let storage = f (parameter, storage) 199 in
  ( (), storage )
