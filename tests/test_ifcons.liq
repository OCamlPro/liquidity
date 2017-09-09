
(* lists *)

(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : string)
      (storage : string list)
      (return : unit) =
      
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
