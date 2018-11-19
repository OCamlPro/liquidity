
type storage = (int, string) variant

let%entry main parameter storage =
  let a = Left parameter in
  [], a
