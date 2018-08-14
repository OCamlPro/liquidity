(* loops *)

[%%version 0.3]

type storage = int

let%entry main
    (parameter : int list)
    (storage : int) =

  let c = 1 in

  let f (x: (int * int)) = x.(1) + c + x.(0) in

  let storage = List.fold f parameter 0 in

  ( ([] : operation list), storage )
