[%%version 0.4]

type storage = (int, tez) map * UnitContract.instance

let%entry main (parameter : UnitContract.instance) storage =
  let foo = storage.(0) in
  let acc =
    Loop.loop (fun acc ->
        let op = Contract.call storage.(1) 1tz () in
        ( false, op :: acc )
      ) []
  in
  acc, storage
