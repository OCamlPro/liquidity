type t = A | B | C

let%entry main
    (parameter : t)
    (storage : unit)
  : (int * unit) =
  let r = match parameter with
    | A -> 1
    | B -> Current.fail ()
    | C -> Current.fail ()
  in

  (* let x = if true then Current.fail() else Current.fail() in *)

  (r, ())
