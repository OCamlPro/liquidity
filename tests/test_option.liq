let contract
  (amount : tez)
  (parameter : int option)
  (storage : unit)
  (return : int) =

  let x = match parameter with
    | None -> 1
    | Some x -> x
  in
  ( x, storage )

