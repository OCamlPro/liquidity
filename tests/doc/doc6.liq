type storage = address

let%entry main _ owner =
  let addr = Current.sender () in
  if addr <> owner then
    Current.failwith ("Sender cannot call");
  ([] : operation list), owner
