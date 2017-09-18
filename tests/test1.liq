
(* let version = "1.0" *)

let contract
      (parameter : timestamp)
      (storage : string * (* 0: S *)
                   timestamp * (* 1: T *)
                     (tez * tez) * (* 2: P N *)
                       (unit,unit) contract * (* 3: X *)
                         (unit,unit) contract * (* 4: A *)
                           (unit,unit) contract)  (* 5: B *)
      (return : (timestamp * tez) * (tez * timestamp) ) =
   let amount = Current.amount () in
   let x = (parameter, amount) in
   let y = (amount, parameter) in
   ( (x,y), storage )
