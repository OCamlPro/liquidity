[%%version 0.3]

let%entry main
    (parameter : unit contract)
    (storage : ((int, tez) map * unit contract)) =
  let foo = storage.(0) in
  let acc =
    Loop.loop (fun acc ->
        let op = Contract.call storage.(1) 1tz () in
        ( false, op :: acc )
      ) ([] : operation list)
  in
  acc, storage
