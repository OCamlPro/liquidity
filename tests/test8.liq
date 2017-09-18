
(* let version = "1.0" *)

let contract
      (parameter : timestamp)
      (storage : tez * tez  (* 2: P N *)
      )
      (return : unit) =
      let p = get storage 1 in
      let storage  = set storage 1 p in
      ( (), storage )
