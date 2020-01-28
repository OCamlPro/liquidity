
type storage = (int, string) variant

let%entry main
      (parameter : string)
      _storage =
  let a = Right parameter in
  ( [], a )
