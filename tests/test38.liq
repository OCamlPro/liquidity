[%%version 0.16]

let%entry main (parameter : int) (storage : tez) : int * tez =
  (- parameter), (2p * storage)
