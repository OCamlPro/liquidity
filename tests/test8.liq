
[%%version 0.13]

let%entry main
      (parameter : timestamp)
      (storage : tez * tez  (* 2: P N *)
      )
      : unit * (tez * tez) =
      let p =
        if parameter = 2017-10-18 then
          storage.(0)
        else
          storage.(1)
      in
      let storage  = set storage 1 p in
      ( (), storage )
