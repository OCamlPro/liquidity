[%%version 0.3]

type storage = (int, tez) map * (contract UnitContract)

let%entry main (parameter : (contract UnitContract)) storage =
  let foo = storage.(0) in
  let acc =
    Loop.loop (fun acc ->
        let op = Contract.call storage.(1) 1tz () in
        ( false, op :: acc )
      ) ([] : operation list)
  in
  acc, storage
