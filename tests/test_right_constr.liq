
let contract
      (parameter : string)
      (storage : (int, string) variant)
      (return : unit) =
  let a = (Right parameter : (int, _) variant) in
  ( (), a )
