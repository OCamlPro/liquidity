[%%version 0.2]

let%entry main
    (parameter : nat contract)
    (storage : int) =

  let r, op = match%nat (storage + 1)  with
    | Plus x ->
      let op = Contract.call parameter 0tz x in
      (x + 2p, op)
    | Minus y ->
      let op = Contract.call parameter 0tz y in
      (y, op)
  in
  let x = r + 10p in
  ([op], int x)
