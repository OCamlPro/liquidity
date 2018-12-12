[%%version 0.4]

type t = A of int | B | C of (int * nat)
type storage = int

let%entry main
    (parameter : UnitContract.instance * t)
    _storage =

  let (c, m) = parameter in
  match m with
  | A i ->
    [], i
  | B ->
    let op = Contract.call c 0tz () in
    [op], 0
  | C _ ->
    let op = Contract.call c 0tz () in
    [op], 0
