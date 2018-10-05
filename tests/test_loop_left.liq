[%%version 0.4]

type storage = int

let%entry main
    (parameter : int)
    (storage : int) =

  let inc0 = 1 in
  let inc1 = 1 in
  let inc = inc0 + inc1 in

  if inc + inc0 + inc1 > 10 then failwith ();

  let storage, rien =
    Loop.left (fun (acc, rien) ->
        let rien = rien + 1 in
        if acc > 10 then ((Right acc : (int, int) variant), rien) (* stop *)
        else ((Left (acc + inc) : (int, int) variant), rien) (* re-loop *)
      ) parameter 0
  in

  if rien = 0 then failwith ();

  let storage =
    Loop.left (fun acc ->
        if acc > 10 then (Right acc : (int, int) variant) (* stop *)
        else (Left (acc + inc) : (int, int) variant) (* re-loop *)
      ) storage
  in

  ( ([] : operation list), storage )
