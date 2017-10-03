
(* lists *)

let version = 1.0

let%entry main
      (parameter : string)
      (storage : string list)
      [%return : unit] =
      
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
        | y :: z -> z
       in

      ( (), storage )
