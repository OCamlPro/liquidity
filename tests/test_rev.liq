(* List.rev *)

let version = 1.0

let%entry main
      (parameter : int)
      (storage : int list)
      [%return : unit] =
  let l = List.rev storage in
  ( (), l )
