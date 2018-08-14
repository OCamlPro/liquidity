
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
   let s = get storage 0 in
   let storage = set storage 0 s in

   ( ([] : operation list), storage )
