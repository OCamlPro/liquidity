[%%version 0.2]

let%entry main
    (parameter : nat contract)
    (storage : bool) =

  let b =
   if
    let op = Contract.call parameter 0tz 1p in
    true
   then
    false
   else
    true
  in
  ([]:operation list), b