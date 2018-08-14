
(* lists *)

[%%version 0.3]

type storage = string list

let%entry main
      (parameter : string)
      (storage : string list) =
      
      let set = ([] : string list) in
      let set = "a" :: set in
      let set = "b" :: set in
      let a = "1" in
      let x = [a;a;a] in
      let storage = if true then
           storage
        else
        match x with
        | [] -> set
        | _ :: z -> z
       in

      ( ([] : operation list), storage )
