
let%entry main
      (parameter : int)
      (storage : (int, string) variant)
      : unit * (int, string) variant =
  let a = (Left parameter : (_, string) variant) in
  ( (), a )
