(* List.map with closure *)

[%%version 0.3]

type storage = int list

let%entry main
      (parameter : int)
      (storage : int list) =
  let add_param (x : int) = x + parameter in
  let l = List.map add_param storage in
  ( ([] : operation list), l )
