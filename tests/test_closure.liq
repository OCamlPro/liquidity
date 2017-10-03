let version = 1.0

let%entry main
      (parameter : int)
      (storage : int)
      [%return : unit] =
  let x = parameter + 10 in
  let f = fun ( arg : int * int ) ->
    arg.(1) + x
  in
  let storage = (parameter, storage) |> f in
  ( (), storage )
