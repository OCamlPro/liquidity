
[%%version 0.11]

let f ((x : unit), (_ : int) ) = x

let%entry main
      (parameter : int)
      (storage : unit)
      : unit * unit =
  let storage = f (storage, parameter) in
  ( (), storage )
