
type storage = (int, string) variant

let%entry main parameter storage =
  let a = Right parameter in
  [], a
