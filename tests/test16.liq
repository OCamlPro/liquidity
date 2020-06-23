
type storage = unit

let%entry default
      (parameter : int)
      (storage : unit) =
  let f = fun arg ->
    arg.(0)
  in
  let storage = (storage, parameter) |> f in
  ( [], storage )

































