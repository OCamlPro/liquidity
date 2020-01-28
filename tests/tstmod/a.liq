type t =
  | A of int
  | B of tez
  | C

type s = int * int

let[@private] y = 0

let[@inline] x = 0

let af (y : int) = y + x
