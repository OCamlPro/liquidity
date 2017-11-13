(* List.map *)

[%%version 0.13]

let succ (x : int) = x + 1

let%entry main
      (parameter : int)
      (storage : int list)
      : unit * int list =
  let l = List.map succ storage in
  ( (), l)

