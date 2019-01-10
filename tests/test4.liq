
[%%version 0.5]
type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                UnitContract.instance * (* 3: X *)
                UnitContract.instance * (* 4: A *)
                UnitContract.instance  (* 5: B *)

let%entry main
      (parameter : timestamp)
      storage =
   let storage = set storage 1 parameter in
   ( [], storage )
