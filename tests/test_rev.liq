(* List.rev *)

[%%version 0.5]

type storage = int list

let%entry main
      (_parameter : int)
      storage =
  let l = List.rev storage in
  ( [], l )
