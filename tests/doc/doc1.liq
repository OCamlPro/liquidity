type storage = dun

let%entry default () s =
  let bal = Current.balance() in
  [], bal
