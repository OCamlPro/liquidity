[%%version 0.4]

type storage = (int, tez) map * UnitContract.instance

let%entry main (_parameter : UnitContract.instance) storage =
  let _foo = storage.(0) in
  let acc =
    Loop.loop (fun acc ->
        let op = Contract.call storage.(1) 1tz () in
        ( false, op :: acc )
      ) []
  in
  acc, storage
