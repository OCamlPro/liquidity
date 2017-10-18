
[%%version 0.12]

type t = tez
type s = (t * tez)

let%entry main
      (parameter : timestamp)
      (storage : s)
      : (int list * (tez * t)) =

      let p = get storage 0 in
      let n = get storage 1 in
      let cost = 1.00tz + p + n in
      ( ([] : int list), storage )
