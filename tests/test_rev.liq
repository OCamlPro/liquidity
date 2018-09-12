(* List.rev *)

[%%version 0.4]

type storage = int list

let%entry main
      (parameter : int)
      (storage : int list) =
  let l = List.rev storage in
  ( ([] : operation list), l )
