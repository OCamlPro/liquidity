[%%version 0.3]
type storage = nat

let%entry main
    (parameter : int)
    (storage : nat) =

  let r = match%nat (parameter + 1)  with
    | Plus x -> x + 2p
    | Minus y -> y + 3p
  in
  let x = r + 10p in
  let w = abs parameter in
  let storage = if w > 0 then x else r in
  (([] : operation list), storage)
