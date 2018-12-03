type storage = timestamp

let%entry main _ _ =
  let now = Current.time() in
  ([] : operation list), now
