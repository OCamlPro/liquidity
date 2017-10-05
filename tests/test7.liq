
[%%version 0.1]

let%entry main
      (parameter : timestamp)
      (storage : (tez * tez)  (* 2: P N *)
      )
      [%return : int list] =

      let p = get storage 0 in
      let n = get storage 1 in
      let cost = 1.00tz + p + n in
      ( ([] : int list), storage )
