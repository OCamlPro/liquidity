[%%version 0.15]

let%entry main
    (parameter : (unit, unit) contract)
    (storage : ((int, tez) map * (unit, unit) contract))
  : unit * ((int, tez) map * (unit, unit) contract) =
  let foo = storage.(0) in
  let storage =
    Loop.loop (fun storage ->
        let _, storage = Contract.call storage.(1) 1tz storage () in
        ( false, storage )
      ) storage
  in
  (), storage
