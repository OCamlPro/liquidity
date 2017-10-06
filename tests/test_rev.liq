(* List.rev *)

[%%version 0.1]

let%entry main
      (parameter : int)
      (storage : int list)
      : unit =
  let l = List.rev storage in
  ( (), l )
