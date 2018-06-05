
[%%version 0.2]

type storage =  string * (* 0: S *)
                timestamp * (* 1: T *)
                (tez * tez) * (* 2: P N *)
                unit contract * (* 3: X *)
                unit contract * (* 4: A *)
                unit contract  (* 5: B *)

let%entry main
      (parameter : timestamp)
      (storage : storage) =
   let s = get storage 0 in
   let storage = set storage 0 s in

   ( ([] : operation list), storage )
