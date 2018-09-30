[%%version 0.3]

type t = A of int | B | C of (int * nat)

let%entry main
    (parameter : unit contract * t)
    (storage : int)  =

  let (c, m) = parameter in
  match m with
  | A _ ->
    let op = Contract.call c 0tz () in
    [op], storage
  | B ->
    let op = Contract.call c 0tz () in
    [op], 0
  | C _ ->
    let op = Contract.call c 1tz () in
    [op], 1
