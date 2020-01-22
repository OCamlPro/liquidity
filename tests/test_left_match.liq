type storage = (string, int) variant

let%entry default
      (parameter : (int, string) variant)
      _storage =

  let s = match parameter with
  | Left left -> Right left
  | Right right -> Left right in
  [], s
