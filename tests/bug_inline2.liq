type storage = unit

let[@inline] y = 1DUN

let[@inline] check_something x =
  if Current.amount () <> x then failwith x


let%entry e1 () () =
  check_something y;
  [], ()

let%entry e2 () () =
  check_something 0DUN;
  [], ()
