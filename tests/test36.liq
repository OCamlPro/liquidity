[%%version 0.15]

let%entry main
    (parameter : unit)
    (storage : ((int, tez) map * (unit, unit) contract))
  : unit * ((int, tez) map * (unit, unit) contract) =
  let foo = storage.(0) in
  let storage =
    Map.fold (fun ((_i, j), storage) ->
        let _, storage = Contract.call storage.(1) j storage () in
        storage
      ) foo storage
  in
  (), storage
