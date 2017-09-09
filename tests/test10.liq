
(* constructors *)

(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : bool)
      (storage :
        bool *
        int option *
        (string,int) map
      )
      (return : unit) =
      
(* options *)
      let x = 3 in
      let option = Some x in
      let storage  = set storage 1 option in

      ( (), storage )
