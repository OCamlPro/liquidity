(* List.rev *)


type storage = int list

let%entry default
      (_parameter : int)
      storage =
  let l = List.rev storage in
  ( [], l )
