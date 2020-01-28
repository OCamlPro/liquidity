
type ('a, 'b) t =
  { x : 'a ; y : 'b }

type ('_a, '_b) storage = ('_a, '_b) t

let%entry test1 () _ =
  [], { y = true ; x = 1 }


let%entry test2 x _ =
  let y = true in
  [], { y ; x }
