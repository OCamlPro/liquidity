type storage = dun

let%entry default () _ =
  let received = Current.amount () in
  [], received
