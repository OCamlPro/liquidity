type storage = tez * int * ((nat * unit) * bool)

let%entry main
    (parameter : unit)
    (storage : storage) =

  let x, y = 0p, 1p in
  let amount, _, ((n, _), b) = storage in
  let storage = set storage 0 amount in
  let storage = set storage 2 (set storage.(2) 0 (set storage.(2).(0) 0 (n + x + y))) in
  (([] : operation list), storage)
