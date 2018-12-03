type storage = tez

let%entry main _ _ =
  let received = Current.amount () in
  ([] : operation list), received
