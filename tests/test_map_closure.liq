(* List.map with closure *)

[%%version 0.12]

let%entry main
      (parameter : int)
      (storage : int list)
      : unit * int list =
  let add_param (x : int) = x + parameter in
  let l = List.map add_param storage in
  ( (), l )
