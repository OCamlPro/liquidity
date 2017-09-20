
(* let version = "1.0" *)

let contract
      (parameter : timestamp)
      (storage : (tez * tez)  (* 2: P N *)
      )
      (return : tez) =

      let p = get storage 0 in
      let n = get storage 1 in
      let cost = 1.00t + p + n in
      ( cost, storage )
