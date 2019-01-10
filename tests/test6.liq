
[%%version 0.5]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                UnitContract.instance * (* 3: X *)
                UnitContract.instance * (* 4: A *)
                UnitContract.instance  (* 5: B *)

let%entry main
      (_parameter : timestamp)
      storage =
   let pn = get storage 2 in
   let storage = set storage 2 pn in
   ( [], storage )
