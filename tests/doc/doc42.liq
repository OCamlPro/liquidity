type storage = unit
let%entry main _ _ =
  let size = List.length [10; 20; 30; 40] in
  if size <> 4p then failwith size;
  ([] : operation list), ()
