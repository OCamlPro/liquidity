(* List.map *)

[%%version 0.3]

let succ (x : int) = x + 1

type storage = int list

let%entry main
      (parameter : int)
      (storage : int list) =
  let l = List.map succ storage in
  ( ([] : operation list), l)
