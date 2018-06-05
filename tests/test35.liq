type t = A | B | C

let%entry main
    (parameter : t)
    (storage : int) =
  let r = match parameter with
    | A -> 1
    | B -> Current.fail ()
    | C -> Current.fail ()
  in

  (* let x = if true then Current.fail() else Current.fail() in *)

  ([] : operation list), r
