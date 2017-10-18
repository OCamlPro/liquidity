
[%%version 0.12]

let%entry main
      (parameter : timestamp)
      (storage : tez * tez  (* 2: P N *)
      )
      : unit * (tez * tez) =
      let p = get storage 1 in
      let storage  = set storage 1 p in
      ( (), storage )
