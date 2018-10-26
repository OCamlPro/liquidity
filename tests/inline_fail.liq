type storage = unit

let[@inline] f (x : int) = failwith x

let%entry main _ s =
  f 1;
  let x = f 0 in
  ([] : operation list), s
