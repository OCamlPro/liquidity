(* Iter *)

[%%version 0.13]

let%entry main
    (parameter : int list)
    (storage : unit)
  : unit * unit =

  List.iter (fun x ->
      if x < 0 then Current.fail ();
    ) parameter;

  let f (x:int) = if x < 0 then Current.fail () in
  List.iter f  parameter;

  ( (), storage )
