type storage = unit

let%entry main _ _ =
  let x = (1, 2, 3, 4) in
  let car = x.(0) in
  let cdr = x.(1) in
  if car <> 1 || cdr <> 2 then failwith "Error !";
  ([] : operation list), ()
