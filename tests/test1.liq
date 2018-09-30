
[%%version 0.3]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                unit contract * (* 3: X *)
                unit contract * (* 4: A *)
                unit contract * (* 5: B *)
                (timestamp * tez) * (tez * timestamp)
let%entry main
      (parameter : timestamp)
      (storage : storage) =
   let amount = Current.amount () in
   let pa = (parameter, amount) in
   let ap = (amount, parameter) in
   let s, t, n, x, a, b, _, _ = storage in
   let storage = s, t, n, x, a, b, pa, ap in
   ( ([] : operation list), storage )
