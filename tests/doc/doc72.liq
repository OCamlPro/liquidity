type storage = int

type ('a, 'b) t =
  { x : 'a ; y : 'b }

let[@noinline] mk_t (x, y) =
  { x; y }

let%entry default parameter _ =
  let w = mk_t (parameter, 99) in
  if not w.x then failwith ();
  let v = mk_t (false, Some 0) in
  if v.x then failwith ();

  [], w.y
