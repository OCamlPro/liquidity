[%%version 0.13]

let%entry main
    (parameter : int)
    (storage : int)
  : nat * int =

  let r = match%nat (parameter + 1)  with
    | Plus x -> x + 2p
    | Minus y -> y + 3p
  in
  let x = r + 10p in
  let storage = abs storage in
  (x, storage)
