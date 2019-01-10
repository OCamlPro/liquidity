[%%version 0.5]

type t = A of int | B | C of (int * nat)
type storage = int

let%entry main
    (parameter : UnitContract.instance * t)
    storage =

  let (c, m) = parameter in
  match m with
  | A i ->
    [], i
  | B ->
    let _op = Contract.call c 0tz () in
    [], 0
  | C (i, _) ->
    let _op = Contract.call c 1tz () in
    [], i
