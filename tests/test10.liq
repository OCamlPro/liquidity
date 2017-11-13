
(* constructors *)

[%%version 0.13]

type s =
        bool *
        int option *
        (string,int) map

let%entry main
      (parameter : bool)
      (storage : s)
      : unit * s =

(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = Array.set storage 1 option in

      ( (), storage )
