type storage = address

let%entry main _ owner =
  let addr = Current.source () in
  if addr <> owner then
    Current.failwith ("Not allowed");
  ([] : operation list), owner
