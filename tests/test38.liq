[%%version 0.2]

type storage = { i: int; t: tez; }

let%entry main (parameter : int) (storage : storage) =
  let s = storage.i <- (- parameter) in
  let s = s.t <- (2p * storage.t) in
  ([] : operation list), s
