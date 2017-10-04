
(* constructors *)

let version = 0.1

let%entry main
      (parameter : bool)
      (storage :
        bool *
        int option *
        (string,int) map
      )
      [%return : unit] =
      
(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = Array.set storage 1 option in

      ( (), storage )
