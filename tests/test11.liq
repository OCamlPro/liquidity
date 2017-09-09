
(* strings *)

(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : string)
      (storage : string)
      (return : unit) =
      
(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in   

      ( (), storage )
