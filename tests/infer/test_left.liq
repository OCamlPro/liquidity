
type storage = int

let%entry main parameter storage =
  let a = Left 3 in
  let b = Right a in

  let storage = match b with
  | Left x -> x
  | Right r ->
     match r with
     | Left x -> x
     | Right _ -> storage
  in

  [], storage

