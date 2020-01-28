type storage = unit
let%entry main () _ =
  let size = List.length [10; 20; 30; 40] in
  if size <> 4p then failwith size;
  [], ()
