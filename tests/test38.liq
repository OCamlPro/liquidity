[%%version 0.151]

let%entry main (parameter : int) (storage : tez) : int * tez =
  (- parameter), (2p * storage)
