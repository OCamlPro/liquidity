[%%version 0.3]

let%entry main
    (parameter : int contract * int option)
    (storage : int) =

  let (c, l) = parameter in
  match l with
  | None ->
    (([] : operation list), storage)
  | Some x  ->
    let op = Contract.call c 0tz x in
    ([op], storage)
