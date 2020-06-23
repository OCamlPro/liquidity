
(* lists *)


type storage = string list

let%entry default
      (_parameter : string)
      storage =
      
      let a = "1" in
      let set = [] in
      let set = a :: set in
      let x = [a;a] in
      let storage =
        match x with
        | [] -> set
        | _ :: z -> z
       in

      ( [], storage )
