
let%entry main
      (parameter : int)
      (storage : (int, string) variant)=
  let a = (Left parameter : (_, string) variant) in
  ( ([]:operation list), a )
