
(* lists *)


type storage = string list

let%entry default
      (_parameter : string)
      (storage : string list) =
      
      let set = [] in
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

      ( [], storage )
