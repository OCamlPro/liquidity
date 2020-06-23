
type storage = int

let%entry default
    (parameter : int)
    storage =

  let inc0 = 1 in
  let inc1 = 1 in
  let inc = inc0 + inc1 in

  if inc + inc0 + inc1 > 10 then failwith ();

  let storage, rien =
    Loop.left (fun (acc, rien) ->
        let rien = rien + 1 in
        if acc > 10 then (Right acc, rien) (* stop *)
        else (Left (acc + inc), rien) (* re-loop *)
      ) parameter 0
  in

  if rien = 0 then failwith ();

  let storage =
    Loop.left (fun acc ->
        if acc > 10 then Right acc (* stop *)
        else Left (acc + inc) (* re-loop *)
      ) storage
  in

  ( [], storage )
