(* Fold *)

[%%version 0.13]

let%entry main
    (parameter : int list)
    (storage : unit)
  : int * unit =

  let res =
    List.fold (fun (x, acc) ->
        if x > acc then x else acc
      )
      [1; 2; 3; 4; 5; 6]
      3
  in

  ( res, storage )
