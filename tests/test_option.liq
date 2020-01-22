
type storage = int

let%entry default
  (parameter : int option)
  _storage =

  let x = match parameter with
    | None -> 1
    | Some x -> x
  in
  ( [], x )
