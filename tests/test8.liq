
[%%version 0.3]

let%entry main
      (parameter : timestamp)
      (storage : tez * tez  (* 2: P N *)
      )
      : operation list * (tez * tez) =
      let p =
        if parameter = 2017-10-18 then
          storage.(0)
        else
          storage.(1)
      in
      let storage  = set storage 1 p in
      ( ([] : operation list), storage )
