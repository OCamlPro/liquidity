type storage = tez

let%entry main _ _ =
  let received = Current.amount () in
  [], received
