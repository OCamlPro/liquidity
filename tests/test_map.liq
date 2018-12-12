(* List.map *)

[%%version 0.4]

let succ x = x + 1

type storage = int list

let%entry main
      (_parameter : int)
      storage =
  let l = List.map succ storage in
  ( [], l)
