
type storage = int

let%entry main
  (parameter : int option)
  (storage : int) =

  let x = match parameter with
    | None -> 1
    | Some x -> x
  in
  ( ([] : operation list), x )
