
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
   let pn = get storage 2 in
   let p = get pn 0 in
   let p = p + 1tz in
   let pn = set pn 0 p in
   let storage = set storage 2 pn in
   ( [], storage )
