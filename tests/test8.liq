

type storage = tez * tez

let%entry default
      (parameter : timestamp)
      storage = (* 2: P N *)
      let p =
        if parameter = 2017-10-18 then
          storage.(0)
        else
          storage.(1)
      in
      let storage  = set storage 1 p in
      ( [], storage )
