
(* strings *)

[%%version 0.14]

let%entry main
      (parameter : string)
      (storage : string)
      : unit * string =

(* options *)
      let storage = if parameter = "" then
           storage
        else
           storage @ parameter
       in

      ( (), storage )
