
type storage = (int, string) variant

let%entry default
      (parameter : int)
      _storage =
  let a = Left parameter in
  ( [], a )
