(* List.map *)

[%%version 0.2]

let succ (x : int) = x + 1

let%entry main
      (parameter : int)
      (storage : int list) =
  let l = List.map succ storage in
  ( ([] : operation list), l)
