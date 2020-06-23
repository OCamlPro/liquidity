
type storage = (int, string) variant

let%entry default
      (parameter : string)
      _storage =
  let a = Right parameter in
  ( [], a )
