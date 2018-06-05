
let%entry main
      (parameter : string)
      (storage : (int, string) variant) =
  let a = (Right parameter : (int, _) variant) in
  ( ([] : operation list), a )
