
let contract
      (parameter : int)
      (storage : (int, string) variant)
      (return : unit) =
  let a = (Left parameter : (_, string) variant) in
  ( (), a )
