
type storage = (string, int) variant

let%entry main parameter storage =

  let s = match parameter with
  | Left left -> Right left
  | Right right -> Left right in
  [], s
