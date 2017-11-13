
[%%version 0.13]

let%entry main
      (parameter : int)
      (storage : int)
      : unit * int =
  let x = parameter + 10 in
  let f = fun ( arg : int * int ) ->
    arg.(1) + x
  in
  let storage = (parameter, storage) |> f in
  ( (), storage )
