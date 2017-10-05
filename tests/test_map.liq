(* List.map *)

[%%version 0.1]

let succ (x : int) = x + 1

let%entry main
      (parameter : int)
      (storage : int list)
      [%return : unit] =
  let l = List.map succ storage in
  ( (), l)

