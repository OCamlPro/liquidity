[%%version 0.13]

let%entry main
    (parameter : (nat, bool) contract)
    (storage : bool * (nat, bool) contract)
  : bool * (bool * (nat, bool) contract) =

  let (b, c) = storage in
  if b then
    false, storage
  else
    let (b, storage) = Contract.call c 0tz storage 0p in
    b, storage
