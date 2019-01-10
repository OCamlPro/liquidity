
[%%version 0.5]

type storage = int

let%entry main
      (parameter : int)
      storage =
  let x = parameter + 10 in
  let f = fun arg ->
    arg.(1) + x
  in
  let storage = (parameter, storage) |> f in
  ( [], storage )
