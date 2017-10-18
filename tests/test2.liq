
[%%version 0.12]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                (unit,unit) contract * (* 3: X *)
                (unit,unit) contract * (* 4: A *)
                (unit,unit) contract  (* 5: B *)

let%entry main
      (parameter : timestamp)
      (storage : storage)
      : ((tez * tez) * (unit,unit) contract) * storage =
   let x = storage.(2) in
   let y = storage.(3) in
   ( (x,y), storage )
