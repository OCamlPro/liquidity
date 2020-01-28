type storage = (string, int) variant

let%entry main
      (parameter : (int, string) variant)
      _storage =

  let s = match parameter with
  | Left left -> Right left
  | Right right -> Left right in
  [], s
