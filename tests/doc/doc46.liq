type storage = int
let%entry main _ _ =
  let (list, acc) = List.map_fold (fun (elt, acc) ->
      ( elt + 1, elt + acc )
    ) [1; 2; 3; 4; 5] 0 in
  ([] : operation list), acc
