
type storage = int

let%entry main
      (_parameter : int)
      storage =
  let a = Left 3 in
  let b = Right a in

  let storage = match b with
  | Left x -> x
  | Right r ->
     match r with
     | Left x -> x
     | Right _ -> storage
  in

  ( [], storage )
