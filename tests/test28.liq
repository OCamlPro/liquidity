[%%version 0.12]

let%entry main
    (parameter : (nat, bool) contract)
    (storage : bool * (nat, bool) contract)
  : bool * (bool * (nat, bool) contract) =

  let (b, c) = storage in
  if b then
    let (b, storage) = Contract.call c 0tz storage 0p in
    b, storage
  else
    false, storage
