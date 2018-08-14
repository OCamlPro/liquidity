(* Iter *)

[%%version 0.3]

type storage = unit

let%entry main
    (parameter : int list)
    (storage : unit) =

  List.iter (fun x ->
      if x < 0 then Current.failwith ();
    ) parameter;

  let f (x:int) = if x < 0 then Current.failwith () in
  List.iter f  parameter;

  ( ([] : operation list), storage )
