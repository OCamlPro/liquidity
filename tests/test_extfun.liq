
[%%version 0.3]

let f ((x : unit), (_ : int) ) = x

type storage = unit

let%entry main
      (parameter : int)
      (storage : unit) =
  let storage = f (storage, parameter) in
  ( ([] : operation list), storage )
