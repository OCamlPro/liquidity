
type storage = ((int, tez) map * address)

let%entry default (_parameter : unit) storage =
  let foo = storage.(0) in
  let acc =
    Map.fold (fun ((_i, j), acc) ->
        let op = Contract.call storage.(1) j default () in
        op :: acc
      ) foo []
  in
  acc, storage
