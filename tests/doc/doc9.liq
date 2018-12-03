type storage = unit

let%entry main _ _ =
  let x = (1,2,3,4) in
  let x0 = x.(0) <- 10 in
  let x1 = x0.(1) <- 11 in
  if x1.(0) <> 10
  || x1.(1) <> 11
  || x1.(2) <> 3
  || x1.(3) <> 4 then failwith "Error !";
  ([] : operation list), ()
