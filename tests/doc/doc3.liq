type storage = dun

let%entry main () _ =
  let received = Current.amount () in
  [], received
