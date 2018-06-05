
[%%version 0.2]

let f ((x : unit), (_ : int) ) = x

let%entry main
      (parameter : int)
      (storage : unit) =
  let storage = f (storage, parameter) in
  ( ([] : operation list), storage )
