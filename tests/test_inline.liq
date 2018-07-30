[%%version 0.35]

(* Will be inlined *)
let f (x:int) = x + 1

(* Won't be inlined *)
let g (x:int) = x + 2

(* Will be inlined *)
let h (x:int) = x + 3 [@@inline]

let%entry main (parameter : unit) (storage : int) =
  let a = f storage in
  let b = g storage in
  let c = g storage in
  let d = h storage in
  let e = h storage in
  ([] : operation list), a + b + c + d + e
