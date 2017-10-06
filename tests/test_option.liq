let%entry main
  (parameter : int option)
  (storage : unit)
  : int * unit =

  let x = match parameter with
    | None -> 1
    | Some x -> x
  in
  ( x, storage )

