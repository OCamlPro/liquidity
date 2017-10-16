type storage = tez * int * ((nat * unit) * bool)

let%entry main
    (parameter : unit)
    (storage : storage)
  :  (nat * tez) * storage =

  let x, y = 0p, 1p in
  let amount, _, ((n, _), b) = storage in
  ((n + x + y, amount), storage)
