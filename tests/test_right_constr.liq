
let%entry main
      (parameter : string)
      (storage : (int, string) variant)
      : unit =
  let a = (Right parameter : (int, _) variant) in
  ( (), a )
