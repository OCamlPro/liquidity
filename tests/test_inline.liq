
(* Will be inlined *)
let f x = x + 1

(* Won't be inlined *)
let g x = x + 2

(* Will be inlined *)
let h x = x + 3 [@@inline]

(* Will not be inlined, even with attribute *)
let op = Contract.call (Contract.self ()) 0tz () [@@inline]

type storage = int

let%entry default (_parameter : unit) storage =
  let a = f storage in
  let b = g storage in
  let c = g storage in
  let d = h storage in
  let e = h storage in
  [op; op], a + b + c + d + e
