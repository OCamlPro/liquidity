[%%version 0.2]

let%entry main
    (parameter : unit)
    (storage : ((int, tez) map * unit contract)) =
  let foo = storage.(0) in
  let acc =
    Map.fold (fun ((_i, j), acc) ->
        let op = Contract.call storage.(1) j () in
        op :: acc
      ) foo ([] : operation list)
  in
  acc, storage
