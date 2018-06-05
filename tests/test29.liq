[%%version 0.2]

let%entry main
    (parameter : int contract * int list)
    (storage : int) =

  let (c, l) = parameter in
  match l with
  | [] ->
    (([]:operation list), storage)
  | x :: _ ->
    let op = Contract.call c 0tz x in
    [op], storage
