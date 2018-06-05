[%%version 0.2]

let%entry main
    (parameter : nat contract)
    (storage : bool * nat contract) =

  let (b, c) = storage in
  if b then
    let op = Contract.call c 0tz 0p in
    [op], set storage 0 true
  else
    ([]:operation list), set storage 0 false
