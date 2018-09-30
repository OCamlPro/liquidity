(* List.rev *)

[%%version 0.3]

let%entry main
      (parameter : int)
      (storage : int list) =
  let l = List.rev storage in
  ( ([] : operation list), l )
