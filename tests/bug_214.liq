type storage = unit

let f () = if true then Current.failwith ()
let g f = f (); ()
let h () = f (); ()

let%entry main () () =
  g f;
  h ();
  [], ()
