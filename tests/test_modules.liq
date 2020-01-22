module A = struct
  type t =
    | A of int
    | B of tez
    | C

  type s = int * int

  let[@private] y = 0

  let[@inline] x = 0

  let af (y : int) = y + x
end

module B = struct
  type t =
    | A of int
    | B of A.t
    | C

  type s = int * int

  module W = struct

    let r = 9
  end

  let bf (y : int) = y + A.x - W.r
end

type storage = B.t


let cf (y : int) = A.af (B.bf (y + A.x)) + B.W.r

let%entry default p s =
  [], B.A (cf p)
