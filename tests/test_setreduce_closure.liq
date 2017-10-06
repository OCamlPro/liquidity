let%entry main
  (parameter : string)
  (storage : tez set)
  : bool =

  let amount = Current.amount() in
  let f (arg: tez * bool) =
    if arg.(0) + amount > 5.0tz then true
    else arg.(1)
  in

  let b = Set.reduce f storage false in
  (b, storage)
