
type ('a, 'b) t =
  { x : 'a ; y : 'b }

type storage = (int, bool) t

let%entry test1 () _ =
  let w = { y = true ; x = 1 } in
  [], w


let%entry test2 x _ =
  let y = true in
  [], { y ; x }
