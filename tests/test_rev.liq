(* List.rev *)

[%%version 0.1]

let%entry main
      (parameter : int)
      (storage : int list)
      [%return : unit] =
  let l = List.rev storage in
  ( (), l )
