[%%version 0.4]

type storage = { i: int; t: tez; }

let%entry main (parameter : int) storage =
  let s = storage.i <- (- parameter) in
  let s = s.t <- (2p * storage.t) in
  [], s
