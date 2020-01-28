
type storage = (int, string) variant

let%entry main
      (parameter : int)
      _storage =
  let a = Left parameter in
  ( [], a )
