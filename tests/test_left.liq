
type storage = int

let%entry main
      (parameter : int)
      (storage : int) =
  let a = (Left 3 : (_, string) variant) in
  let b = (Right a : (int, _) variant) in

  let storage = match b with
  | Left x -> x
  | Right r ->
     match r with
     | Left x -> x
     | Right _ -> storage
  in

  ( ([] : operation list), storage )
