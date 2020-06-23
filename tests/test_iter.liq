(* Iter *)


type storage = unit

let%entry default
    (parameter : int list)
    storage =

  List.iter (fun x ->
      if x < 0 then Current.failwith ();
    ) parameter;

  let f x = if x < 0 then Current.failwith () in
  List.iter f  parameter;

  ( [], storage )
