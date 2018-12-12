(* loops *)

[%%version 0.4]

type storage = int

let%entry main
    (parameter : int list)
    storage =

  let c = 1 in

  let f x = x.(1) + c + x.(0) in

  let storage = List.fold f parameter 0 in

  ( [], storage )
