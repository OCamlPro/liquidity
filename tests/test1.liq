
[%%version 0.5]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                UnitContract.instance * (* 3: X *)
                UnitContract.instance * (* 4: A *)
                UnitContract.instance * (* 5: B *)
                (timestamp * tez) * (tez * timestamp)
let%entry main
      (parameter : timestamp)
      (storage : storage) =
   let amount = Current.amount () in
   let pa = (parameter, amount) in
   let ap = (amount, parameter) in
   let s, t, n, x, a, b, _, _ = storage in
   let storage = s, t, n, x, a, b, pa, ap in
   ( [], storage )
