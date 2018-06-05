
(* constructors *)

[%%version 0.2]

type s =
        bool *
        int option *
        (string,int) map

let%entry main
      (parameter : bool)
      (storage : s)
      : operation list * s =

(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = Array.set storage 1 option in

      ( ([] : operation list), storage )
