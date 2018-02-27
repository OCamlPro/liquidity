[%%version 0.15]

let%entry main (parameter : int) (storage : tez) : int * tez =
  (- parameter), (2p * storage)
