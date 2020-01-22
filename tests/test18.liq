type storage = int * nat

let%entry default
      (parameter : int)
      (storage : (int * nat)) =

  let s = match int storage.(1) / parameter with
    | None -> (0, 0p)
    | Some res ->
      ( res.(0), res.(1) ) in
  ( [], s )
