type storage = tez

let%entry main _ s =
  let bal = Current.balance() in
  ([] : operation list), bal
