type storage = tez * int * ((nat * unit) * bool)

let%entry default
    (_parameter : unit)
    storage =

  let x, y = 0p, 1p in
  let amount, _, ((n, _), _b) = storage in
  let storage = set storage 0 amount in
  let storage = set storage 2 (set storage.(2) 0 (set storage.(2).(0) 0 (n + x + y))) in
  ([], storage)
