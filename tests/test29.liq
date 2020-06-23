
type storage = int

let%entry default
    (parameter : address * int list)
    storage =

  let (c, l) = parameter in
  match l with
  | [] ->
    ([], storage)
  | x :: _ ->
    let op = Contract.call c ~entry:default 0tz x in
    [op], storage
