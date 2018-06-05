
(* lists *)

[%%version 0.2]

let%entry main
      (parameter : string)
      (storage : string list) =
      
      let a = "1" in
      let set = ([] : string list) in
      let set = a :: set in
      let x = [a;a] in
      let storage =
        match x with
        | [] -> set
        | _ :: z -> z
       in

      ( ([] : operation list), storage )
