
[%%version 0.4]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                UnitContract.instance * (* 3: X *)
                UnitContract.instance * (* 4: A *)
                UnitContract.instance  (* 5: B *)

let%entry main
      (_parameter : timestamp)
      storage =
   let pn = storage.(2) in
   let x = storage.(3) in
   let s, t, _, _, a, b = storage in
   let storage = s, t, pn, x, a, b in

   ( [], storage )
