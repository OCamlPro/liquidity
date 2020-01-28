type storage = dun

let%entry main () s =
  let bal = Current.balance() in
  [], bal
