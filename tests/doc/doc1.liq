type storage = tez

let%entry main () s =
  let bal = Current.balance() in
  [], bal
