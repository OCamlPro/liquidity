[%%version 0.12]

let%entry main
    (parameter : (int, unit) contract * int option)
    (storage : int)
  : unit * int =

  let (c, l) = parameter in
  match l with
  | None ->
    ((), storage)
  | Some x  ->
    let (result, storage) = Contract.call c 0tz storage x in
    (result, storage)
