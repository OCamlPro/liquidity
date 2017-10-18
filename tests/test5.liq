
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
      : (unit * storage) =
   let pn = get storage 2 in
   let p = get pn 0 in
   let p = p + 1tz in
   let pn = set pn 0 p in
   let storage = set storage 2 pn in
   ( (), storage )
