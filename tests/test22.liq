[%%version 0.3]

type t = A of int | B | C of (int * nat)
type storage = int

let%entry main
    (parameter : (contract UnitContract) * t)
    (storage : int) =

  let (c, m) = parameter in
  match m with
  | A i ->
    ([]:operation list), i
  | B ->
    let op = Contract.call c 0tz () in
    ([]:operation list), 0
  | C (i, _) ->
    let op = Contract.call c 1tz () in
    ([]:operation list), i
