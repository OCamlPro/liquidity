(* Fold *)

[%%version 0.13]

let%entry main
    (parameter : int list)
    (storage : unit)
  : int * unit =

  let res =
    List.fold (fun arg (* (x, acc) *) ->
        let x, acc = arg in
        (* acc *)
        if x > acc then x else acc
      )
      [1; 2; 3; 4; 5; 6]
      3
      (* "c" *)
  in

  ( res, storage )
