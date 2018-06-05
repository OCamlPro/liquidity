
[%%version 0.2]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                unit contract * (* 3: X *)
                unit contract * (* 4: A *)
                unit contract  (* 5: B *)

let%entry main
      (parameter : timestamp)
      (storage : storage) =
   let pn = storage.(2) in
   let x = storage.(3) in
   let s, t, _, _, a, b = storage in
   let storage = s, t, pn, x, a, b in

   ( ([] : operation list), storage )
