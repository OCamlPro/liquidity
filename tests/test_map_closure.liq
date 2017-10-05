(* List.map with closure *)

let version = 1.0

let%entry main
      (parameter : int)
      (storage : int list)
      [%return : unit] =
  let add_param (x : int) = x + parameter in
  let l = List.map add_param storage in
  ( (), l )
