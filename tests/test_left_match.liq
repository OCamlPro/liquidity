
let%entry main
      (parameter : (int, string) variant)
      (storage : int)
      : string * int =

  match parameter with
  | Left left -> ("", left)
  | Right right -> (right, storage)
