[%%version 0.2]

let%entry main
    (parameter : int contract * int list)
    (storage : int) =

  let (c, l) = parameter in
  match l with
  | [] ->
    let op = Contract.call c 0tz (-1) in
    ([op], storage)
  | _ :: _ ->
    (([] : operation list), storage)
