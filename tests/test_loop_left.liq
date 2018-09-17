[%%version 0.4]

type storage = int

let%entry main
    (parameter : int)
    (storage : int) =

  let inc0 = 1 in
  let inc1 = 1 in
  let inc = inc0 + inc1 in

  if inc + inc0 + inc1 > 10 then failwith ();

  let storage =
    Loop.left (fun acc ->
        if acc > 10 then (Right acc : (int, int) variant) (* stop *)
        else (Left (acc + inc) : (int, int) variant) (* re-loop *)
      ) (Left parameter : (int, int) variant)
  in

  ( ([] : operation list), storage )
