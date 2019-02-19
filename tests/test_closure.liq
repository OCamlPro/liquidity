
[%%version 0.5]

type storage = int

let%entry main
      (parameter : int)
      storage =
  let x = parameter + 10 in
  let f = fun (_, arg) ->
    arg + x
  in
  let storage = (parameter, storage) |> f in
  ( [], storage )
