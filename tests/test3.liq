
(* let version = "1.0" *)

let contract
      (amount : tez)
      (parameter : timestamp)
      (storage : string * (* 0: S *)
                   timestamp * (* 1: T *)
                     (tez * tez) * (* 2: P N *)
                       (unit,unit) contract * (* 3: X *)
                         (unit,unit) contract * (* 4: A *)
                           (unit,unit) contract)  (* 5: B *)
      (return : unit ) =
   let s = get storage 0 in
   let storage = set storage 0 s in
   ( (), storage )
