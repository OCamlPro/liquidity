
type storage =
| Nothing
| Int of int
| String of string * int

let%entry default
      (_parameter : int)
      storage =
  let _a = Nothing in
  let b = Int 3 in
  let c = String ("toto",0) in

  let storage = match c with
  | String s -> if s.(0) = "toto" then Int 0 else Int 1
  | Int n -> Int n
  | Nothing -> b
  in

  ( [], storage )

































