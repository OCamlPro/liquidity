
[%%version 0.3]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                (contract UnitContract) * (* 3: X *)
                (contract UnitContract) * (* 4: A *)
                (contract UnitContract)  (* 5: B *)

let%entry main
      (parameter : timestamp)
      (storage : storage) =
   let pn = storage.(2) in
   let x = storage.(3) in
   let s, t, _, _, a, b = storage in
   let storage = s, t, pn, x, a, b in

   ( ([] : operation list), storage )
