
(* strings *)

[%%version 0.1]

let%entry main
      (parameter : string)
      (storage : string)
      : unit =
      
(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in   

      ( (), storage )
