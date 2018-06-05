
(* strings *)

[%%version 0.2]

let%entry main
      (parameter : string)
      (storage : string) =

(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in

      ( ([] : operation list), storage )
