
let%entry main
      (parameter : int)
      (storage : (int, string) variant)
      : unit =
  let a = (Left parameter : (_, string) variant) in
  ( (), a )
