type storage = (string, int) variant

let%entry main
      (parameter : (int, string) variant)
      (storage : storage) =

  let s = match parameter with
  | Left left -> (Right left : (string, _) variant)
  | Right right -> (Left right  : (_, int) variant) in
  ([] : operation list), s
