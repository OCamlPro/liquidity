
(* constructors *)

[%%version 0.3]

type storage =
        bool *
        int option *
        (string,int) map

let%entry main
      (parameter : bool)
      (storage : storage)
      : operation list * storage =

(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = Array.set storage 1 option in

      ( ([] : operation list), storage )
