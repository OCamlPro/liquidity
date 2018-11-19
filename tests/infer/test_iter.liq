(* Iter *)

[%%version 0.4]

type storage = unit

let%entry main parameter storage =

  List.iter (fun x ->
      if x < 0 then Current.failwith ();
    ) parameter;

  let f x = if x < 0 then Current.failwith () in
  List.iter f  parameter;


  [], storage
