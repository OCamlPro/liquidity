(* loops *)

[%%version 0.5]

type storage = int

let%entry main
    (parameter : int list)
    storage =

  let c = 1 in

  let f (a, b) = b + c + a in

  let storage = List.fold f parameter 0 in

  ( [], storage )
