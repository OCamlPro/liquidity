
type storage = (int, tez) map * address

let%entry default (_parameter : address) storage =
  let _foo = storage.(0) in
  let acc =
    Loop.loop (fun acc ->
        let op = Contract.call storage.(1) 1tz default () in
        ( false, op :: acc )
      ) []
  in
  acc, storage
