[%%version 0.12]

let%entry main
    (parameter : (nat, nat) contract)
    (storage : int)
  : nat * int =

  let r, storage = match%nat (storage + 1)  with
    | Plus x -> (x + 2p, storage)
    | Minus y ->
      let (w, storage) = Contract.call parameter 0tz storage y in
      (w, storage)
  in
  let x = r + 10p in
  let storage = abs storage in
  (x, storage)
