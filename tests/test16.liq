
type storage = unit

let%entry default
      (parameter : int)
      (storage : unit) =
  let f = fun (s, _) ->
    s
  in
  let storage = (storage, parameter) |> f in
  ( [], storage )

































