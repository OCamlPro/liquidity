[%%version 0.12]

let%entry main
    (parameter : (nat, bool) contract)
    (storage : bool)
  : bool * bool =

  if
    let (b, _) = Contract.call parameter 0tz storage 1p in
    b
  then
    false, false
  else
    true, false
