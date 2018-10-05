type t = A | B | C

type storage = int

let%entry main
    (parameter : t)
    (storage : int) =
  let r = match parameter with
    | A -> 1
    | B -> Current.failwith ()
    | C -> Current.failwith ()
  in

  (* let x = if true then Current.fail() else Current.fail() in *)

  ([] : operation list), r
