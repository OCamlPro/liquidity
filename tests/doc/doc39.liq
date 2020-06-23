type storage = unit
let%entry default () _ =
  let square x = x * x in
  let x = 23 |> square in
  let y = square 23 in (* this is the same as x *)
  let z = square @@ 23 in (* this is also the same as x *)
  if x <> y || x <> z then failwith (x, y, z);
  [], ()
