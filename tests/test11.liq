
(* strings *)

[%%version 0.4]

type storage = string

let%entry main
      (parameter : string)
      (storage : string) =

(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in

      ( [], storage )
