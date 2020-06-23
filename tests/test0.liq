

type storage = string * (* 0: S *)
               timestamp * (* 1: T *)
               (tez * tez) * (* 2: P N *)
               address * (* 3: X *)
               address * (* 4: A *)
               address  (* 5: B *)
let%entry default
      (_parameter : timestamp)
      (storage : storage) =
   ( [], storage )
