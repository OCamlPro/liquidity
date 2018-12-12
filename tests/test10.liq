
(* constructors *)

[%%version 0.4]

type storage =
        bool *
        int option *
        (string,int) map

let%entry main
      (_parameter : bool)
      (storage : storage) =

(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = Array.set storage 1 option in

      ( [], storage )
