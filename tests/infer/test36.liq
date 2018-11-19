[%%version 0.4]

type storage = ((int, tez) map * UnitContract.instance)

let%entry main parameter storage =
  let foo = storage.(0) in
  let acc =
    Map.fold (fun ((_i, j), acc) ->
        let op = Contract.call storage.(1) j () in
        op :: acc
      ) foo []
  in
  acc, storage
