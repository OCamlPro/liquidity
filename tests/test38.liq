
type storage = { i: int; t: tez; }

let%entry default (parameter : int) storage =
  let s = storage.i <- (- parameter) in
  let s = s.t <- (2p * storage.t) in
  [], s
