type t =
  | A of int
  | B of tez
  | C

type s = int * int

module W = struct

   let r = 9
end

let bf (y : int) = y + A.x - W.r
