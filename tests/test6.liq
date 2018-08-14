
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
   let pn = get storage 2 in
   let storage = set storage 2 pn in
   ( ([] : operation list), storage )
