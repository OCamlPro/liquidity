(* List.map *)


let succ x = x + 1

type storage = int list

let%entry default
      (_parameter : int)
      storage =
  let l = List.map succ storage in
  ( [], l)
