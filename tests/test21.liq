[%%version 0.12]

let%entry main
    (parameter : int)
    (storage : int)
  : nat * int =

  let r = match%abs (parameter + 1)  with
    | Plus x -> x
    | Minus y -> y
  in

  (r, storage)
