type storage = unit
let%entry default () _ =
  let sum = List.fold (fun (elt, acc) ->
      elt + acc
    ) [1; 2; 3; 4; 5] 0
  in
  if sum <> 15 then failwith sum;
  [], ()
