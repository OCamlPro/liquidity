[%%version 0.12]

let%entry main
    (parameter : (int, unit) contract * int list)
    (storage : int)
  : unit * int =

  let (c, l) = parameter in
  match l with
  | [] ->
    let (result, storage) = Contract.call c 0tz storage (-1) in
    (result, storage)
  | _ :: _ ->
    ((), storage)
