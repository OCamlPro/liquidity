type storage = tez

let%entry main () _ =
  let received = Current.amount () in
  [], received
