
(* lists *)

[%%version 0.1]

let%entry main
      (parameter : string)
      (storage : string list)
      : unit =
      
      let a = "1" in
      let set = ([] : string list) in
      let set = a :: set in
      let x = [a;a] in
      let storage =
        match x with
        | [] -> set
        | y :: z -> z
       in

      ( (), storage )
