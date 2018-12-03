type storage = unit
let%entry main _ _ =
  let square (x : int) = x * x in
  let x = 23 |> square in
  let y = square 23 in (* this is the same as x *)
  if x <> y then failwith (x, y);
  ([] : operation list), ()
