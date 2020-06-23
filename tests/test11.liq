
(* strings *)


type storage = string

let%entry default
      (parameter : string)
      (storage : string) =

(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in

      ( [], storage )
