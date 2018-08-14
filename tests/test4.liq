
[%%version 0.3]
type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                (contract UnitContract) * (* 3: X *)
                (contract UnitContract) * (* 4: A *)
                (contract UnitContract)  (* 5: B *)

let%entry main
      (parameter : timestamp)
      (storage : storage)
      : (operation list * storage) =
   let storage = set storage 1 parameter in
   ( ([] : operation list), storage )
